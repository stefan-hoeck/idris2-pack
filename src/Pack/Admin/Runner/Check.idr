module Pack.Admin.Runner.Check

import Control.Monad.State
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

toState : HasIO io => EitherT err io a -> StateT s io (Either err a)
toState = lift . runEitherT

updateRep :  HasIO io
          => PkgName
          -> Report
          -> StateT ReportDB io Report
updateRep p rep = modify (insert p rep) $> rep

report :  HasIO io
       => PkgName
       -> SafeLib
       -> EitherT PackErr io ()
       -> StateT ReportDB io Report
report p rp act = do
  Right _ <- toState act | Left _ => updateRep p (Failure rp [])
  updateRep p (Success rp)

missing : ResolvedLib t -> ResolvedLib t
missing = {status := Missing}

covering
testPkg :  HasIO io
        => (e : IdrisEnv)
        => LibCache
        => SafeLib
        -> EitherT PackErr io ()
testPkg (RL pkg n _ _ _) = case pkg of
  GitHub u c _ _ (Just t) => do
    d <- withGit n u c pure
    runIpkg (d </> t) [] e
  Local d _ _ (Just t)    => runIpkg (d </> t) [] e
  _                       =>
    info "No tests to run for \{n}"

covering
checkPkg :  HasIO io
         => IdrisEnv
         => LibCache
         => PkgName
         -> StateT ReportDB io Report
checkPkg p = do
  Nothing  <- lookup p <$> get | Just rep => pure rep
  info "Checking \{p}"
  Right rl <- toState $ resolveLib p >>= safeLib . missing
    | Left err => warn "Could not resolve \{p}" >>
                  updateRep p (Error p err)
  [] <- failingDeps <$> traverse checkPkg (dependencies rl)
    | rs => warn "\{p} had failing dependencies" >>
            updateRep p (Failure rl rs)
  Right () <- toState $ installAny $ Lib rl
    | Left err => warn "Failed to build \{p}" >>
                  updateRep p (Failure rl [])
  Right () <- toState $ testPkg rl
    | Left err => warn "Test failures in \{p}" >>
                  updateRep p (Failure rl [])
  updateRep p (Success rl)

covering
copyDocs :  HasIO io
         => IdrisEnv
         => LibCache
         => File Abs
         -> ReportDB
         -> EitherT PackErr io ()
copyDocs f db = traverse_ go db
  where go : Report -> EitherT PackErr io ()
        go (Success rl) = do
            installDocs rl
            copyDir (pkgDocs rl.name rl.pkg)
                    (f.parent /> "docs" <//> name rl)
        go _            = pure ()

export covering
checkDB : HasIO io => File Abs -> IdrisEnv -> EitherT PackErr io ()
checkDB p e =
  let ps := map (MkPkgName . interpolate) corePkgs
         ++ keys e.env.db.packages
   in do
     ref <- emptyCache
     install [(App False, "katla")]
     rep <- liftIO $ execStateT empty
                   $ traverse_ checkPkg ps
     write p (printReport rep)
     copyDocs p rep
     case numberOfFailures rep of
       0 => pure ()
       n => throwE (BuildFailures n)
