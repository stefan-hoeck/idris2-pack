module Pack.Admin.Runner.Check

import Data.Either
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
import System.Info

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
missing rl = {status := Missing rl.hash} rl

covering
test :
     {auto _ : HasIO io}
  -> {auto e : IdrisEnv}
  -> SafeLib
  -> EitherT PackErr io TestResult
test (RL pkg h n d _ _) =
  case e.env.config.skipTests of
    True  => pure Skipped
    False => case pkg of
      Git u c _ _ (Just t) _ _ => do
        d <- withGit n u c False pure
        runIpkg (d </> t) [] e
        pure TestSuccess
      Local d _ _ (Just t) => runIpkg (d </> t) [] e $> TestSuccess
      _                    => info "No tests to run for \{n}" $> NoTests

||| Call test and update report.
covering
callTestAndUpdateReport :
     {auto _ : HasIO io}
  -> {auto _ : IdrisEnv}
  -> {auto _ : ReportDB}
  -> PkgName
  -> SafeLib
  -> io ()
callTestAndUpdateReport n pkg = do
  Right res <- runEitherT $ test pkg
    | Left _ => do
        warn "Tests for package \{n} failed"
        updateRep_ n (Success pkg TestFailure)
  updateRep_ n (Success pkg res)

||| Ensure that we are testing a package against the
||| operating system(s) it is intended to run on.
covering
testPlatform :
     {auto _ : HasIO io}
  -> {auto _ : IdrisEnv}
  -> {auto _ : ReportDB}
  -> PkgName
  -> SafeLib
  -> io ()
testPlatform n (RL (Core _) _ name _ _ _)                         = do
  warn "Invalid SafeLib (ResolvedLib Safe) type (Core)"
  pure ()
testPlatform n pkg@(RL (Git _ _ _ _ _ _ (Just mts)) _ name _ _ _) =
  if -- ensure package can be tested against current os
     any platformOf mts
     then callTestAndUpdateReport n pkg
     else do
       info """
         Trying to run the following package's test suite on an
         incompatible machine type (\{toPlatform os}): \{name}
       """
       pure ()
testPlatform n pkg@(RL (Git _ _ _ _ _ _ Nothing)    _ name _ _ _) =
  -- assume package was intended to be tested against all oses
  callTestAndUpdateReport n pkg
testPlatform _ (RL (Local _ _ _ _) _ name _ _ _)                  = do
  warn "Invalid SafeLib (ResolvedLib Safe) type (Local)"
  pure ()

covering
testPkg :
     {auto _  : HasIO io}
  -> {auto e  : IdrisEnv}
  -> {auto db : ReportDB}
  -> PkgName
  -> io ()
testPkg n = do
  Just (Success pkg NoTests) <- getRep n | _ => pure ()
  testPlatform n pkg

mutual
  ||| Ensure no failing dependencies, install dependencies for the package
  ||| and update report.
  covering
  ensureNoFailingDepsInstallLibsAndUpdateReport :
         {auto _ : HasIO io}
      -> {auto _ : IdrisEnv}
      -> {auto _ : ReportDB}
      -> PkgName
      -> SafeLib
      -> io Report
  ensureNoFailingDepsInstallLibsAndUpdateReport p rl = do
    reps <- rights <$> traverse checkPkg (dependencies rl)
    []   <- failingDeps <$> pure reps
      | rs => warn "\{p} had failing dependencies" >>
              updateRep p (Failure rl rs)
    Right () <- runEitherT $ installLibs [p]
      | Left err => warn "Failed to build \{p}" >>
                    updateRep p (Failure rl [])
    updateRep p (Success rl NoTests)

  covering
  checkPkg :
       {auto _ : HasIO io}
    -> {auto _ : IdrisEnv}
    -> {auto _ : ReportDB}
    -> PkgName
    -> io (Either () Report)
  checkPkg p = do
    Nothing  <- getRep p | Just rep => pure $ Right rep
    info "Checking \{p}"
    Right rl <- runEitherT $ resolveLib p >>= safeLib . missing
      | Left err => do
          warn "Could not resolve \{p}"
          rep <- updateRep p (Error p err)
          pure $ Right rep
    checkPlatform p rl

  ||| Ensure that we are checking a package against the
  ||| operating system(s) it is intended to run on.
  covering
  checkPlatform :
       {auto _ : HasIO io}
    -> {auto _ : IdrisEnv}
    -> {auto _ : ReportDB}
    -> PkgName
    -> SafeLib
    -> io (Either () Report)
  checkPlatform _ (RL (Core _) _ name _ _ _)                        = do
    warn "Invalid SafeLib (ResolvedLib Safe) type (Core)"
    pure $ Left ()
  checkPlatform p rl@(RL (Git _ _ _ _ _ _ (Just mts)) _ name _ _ _) =
    if -- ensure package can be built against current os
       any platformOf mts
       then do
         rep <- ensureNoFailingDepsInstallLibsAndUpdateReport p rl
         pure $ Right rep
       else do
         info """
           Trying to run the following package's test suite on an
           incompatible machine type (\{toPlatform os}): \{name}
           """
         pure $ Left ()
  checkPlatform p rl@(RL (Git _ _ _ _ _ _ Nothing)    _ name _ _ _) = do
    -- assume package was intended to be built against all oses
    rep <- ensureNoFailingDepsInstallLibsAndUpdateReport p rl
    pure $ Right rep
  checkPlatform _ (RL (Local _ _ _ _) _ name _ _ _)                 = do
    warn "Invalid SafeLib (ResolvedLib Safe) type (Local)"
    pure $ Left ()

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
    copyDirInto (pkgDocs rl.name rl.hash rl.pkg rl.desc) (p </> "docs" <//> n)
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
