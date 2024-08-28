module Pack.Admin.Runner.Check

import Data.IORef
import Data.SortedMap
import Idris.Package.Types
import Pack.Admin.Report.Types
import Pack.Config
import Pack.Core
import Pack.Database
import Pack.Runner.Database
import Pack.Runner.Develop
import Pack.Runner.Install

%default total

--------------------------------------------------------------------------------
--          State
--------------------------------------------------------------------------------

public export
0 ReportDB : Type
ReportDB = IORef (SortedMap PkgName Report)

updateRep : HasIO io => {auto db : ReportDB} -> PkgName -> Report -> io Report
updateRep p rep = modifyIORef db (insert p rep) $> rep

updateRep_ : HasIO io => {auto db : ReportDB} -> PkgName -> Report -> io ()
updateRep_ p = ignore . updateRep p

getRep : HasIO io => {auto db : ReportDB} -> PkgName -> io (Maybe Report)
getRep p = lookup p <$> readIORef db

missing : ResolvedLib t -> ResolvedLib t
missing = {status := Missing}

covering
test :
     {auto _ : HasIO io}
  -> {auto e : IdrisEnv}
  -> SafeLib
  -> EitherT PackErr io TestResult
test (RL pkg n d _ _) =
  case e.env.config.skipTests of
    True  => pure Skipped
    False => case  pkg of
      Git u c _ _ (Just t) => do
        d <- withGit n u c pure
        runIpkg (d </> t) [] e
        pure TestSuccess
      Local d _ _ (Just t) => runIpkg (d </> t) [] e $> TestSuccess
      _                    => info "No tests to run for \{n}" $> NoTests

covering
testPkg :
     {auto _  : HasIO io}
  -> {auto e  : IdrisEnv}
  -> {auto db : ReportDB}
  -> PkgName
  -> io ()
testPkg n = do
  Just (Success pkg NoTests) <- getRep n | _ => pure ()
  Right res <- runEitherT $ test pkg
    | Left _ => do
        warn "Tests for package \{n} failed"
        updateRep_ n (Success pkg TestFailure)
  updateRep_ n (Success pkg res)

covering
checkPkg :
     {auto _ : HasIO io}
  -> {auto _ : IdrisEnv}
  -> {auto _ : ReportDB}
  -> PkgName
  -> io Report
checkPkg p = do
  Nothing  <- getRep p | Just rep => pure rep
  info "Checking \{p}"
  Right rl <- runEitherT $ resolveLib p >>= safeLib . missing
    | Left err => warn "Could not resolve \{p}" >>
                  updateRep p (Error p err)
  [] <- failingDeps <$> traverse checkPkg (dependencies rl)
    | rs => warn "\{p} had failing dependencies" >>
            updateRep p (Failure rl rs)
  Right () <- runEitherT $ installLibs [p]
    | Left err => warn "Failed to build \{p}" >>
                  updateRep p (Failure rl [])
  updateRep p (Success rl NoTests)

covering
checkAll :
     {auto _ : HasIO io}
  -> {auto _  : IdrisEnv}
  -> {auto db : ReportDB}
  -> List PkgName
  -> io (List Report)
checkAll xs = do
  traverse_ checkPkg xs
  traverse_ testPkg xs
  values <$> readIORef db

covering
docsFor :
     {auto _ : HasIO io}
  -> {auto e : IdrisEnv}
  -> Path Abs
  -> PkgName
  -> io (Maybe String)
docsFor p n = do
  res  <- runEitherT $ do
    rl <- resolveLib n
    copyDirInto (pkgDocs rl.name rl.pkg rl.desc) (p </> "docs" <//> n)
  case res of
    Left _  => pure (Just "\{n}")
    Right _ => pure Nothing

allPkgs : {auto e : IdrisEnv} -> List PkgName
allPkgs = map corePkgName corePkgs ++ keys e.env.db.packages

covering
makeDocs : HasIO io => Path Abs -> IdrisEnv -> EitherT PackErr io ()
makeDocs p e = do
  mkDir (p </> "docs")
  ps <- catMaybes <$> traverse (docsFor p) allPkgs
  logMany Warning "Generating the docs of the following packages failed:" ps

export covering
checkDB : HasIO io => Path Abs -> (e : IdrisEnv) -> EitherT PackErr io ()
checkDB p e = do
  ref <- newIORef (the (SortedMap PkgName Report) empty)
  rep <- checkAll allPkgs
  logMany Info "The following packages built successfully:"
    (mapMaybe successLine rep)
  logMany Warning "The following packages failed to build:"
    (mapMaybe failureLine rep)
  logMany Warning "The following packages could not be resolved:"
    (mapMaybe resolveLine rep)
  write (p /> "STATUS.md") (printReport rep)

  let dbName := cast {to = Body} e.env.config.collection <+> ".toml"
  case numberOfFailures rep of
    0 =>
      copyFile (dbDir /> dbName) (p /> dbName) >>
      when e.env.config.withDocs (makeDocs p e)
    n => warn (printErr $ BuildFailures n)
