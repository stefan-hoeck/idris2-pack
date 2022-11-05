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
      HasIO io
  => {auto e : IdrisEnv}
  -> SafeLib
  -> EitherT PackErr io TestResult
test (RL pkg n d _ _) = case  pkg of
  GitHub u c _ _ (Just t) => do
    d <- withGit n u c pure
    runIpkg (d </> t) [] e
    pure TestSuccess
  Local d _ _ (Just t)    => runIpkg (d </> t) [] e $> TestSuccess
  _                       => info "No tests to run for \{n}" $> NoTests

covering
testPkg :
     HasIO io
  => {auto e  : IdrisEnv}
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
     HasIO io
  => {auto _ : IdrisEnv}
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
  Right () <- runEitherT $ installLib rl
    | Left err => warn "Failed to build \{p}" >>
                  updateRep p (Failure rl [])
  updateRep p (Success rl NoTests)

covering
checkAll :
     HasIO io
  => {auto _  : IdrisEnv}
  -> {auto db : ReportDB}
  -> List PkgName
  -> io (SortedMap PkgName Report)
checkAll xs = do
  traverse_ checkPkg xs
  traverse_ testPkg xs
  readIORef db

export covering
checkDB : HasIO io => File Abs -> IdrisEnv -> EitherT PackErr io ()
checkDB p e =
  let ps := map (MkPkgName . interpolate) corePkgs
         ++ keys e.env.db.packages
   in do
     ref <- newIORef (the (SortedMap PkgName Report) empty)
     rep <- checkAll ps
     write p (printReport rep)
     case numberOfFailures rep of
       0 => pure ()
       n => throwE (BuildFailures n)
