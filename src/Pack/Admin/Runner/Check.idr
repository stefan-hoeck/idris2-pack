module Pack.Admin.Runner.Check

import Control.Monad.State
import Data.SortedMap
import Pack.Admin.Report.Types
import Pack.Config.Env
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types
import Pack.Runner.Database
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
       -> ResolvedPackage
       -> EitherT PackErr io ()
       -> StateT ReportDB io Report
report p rp act = do
  Right _ <- toState act | Left _ => updateRep p (Failure rp [])
  updateRep p (Success rp)

covering
checkPkg :  HasIO io
         => Env HasIdris
         -> PkgName
         -> StateT ReportDB io Report
checkPkg e p = do
  info e "Checking \{p}"
  Nothing  <- lookup p <$> get | Just rep => pure rep
  Right rp <- toState $ resolve e (Pkg p)
    | Left err => updateRep p (Error p err)
  [] <- failingDeps <$> traverse (checkPkg e) (depNames rp)
    | rs => updateRep p (Failure rp rs)
  case rp of
    RGitHub pn url commit ipkg d =>
      report p rp $ withGit (tmpDir e) url commit $ do
        let pf = patchFile e pn ipkg
        when !(exists pf) (patch ipkg pf)
        idrisPkg e (packageInstallPrefix e rp) "--install-with-src" ipkg

    RIpkg ipkg d =>
      report p rp $ idrisPkg e (packageInstallPrefix e rp) "--install" ipkg

    RLocal _ dir ipkg d => do
      report p rp $ inDir dir $
        idrisPkg e (packageInstallPrefix e rp) "--install" ipkg

    _ => updateRep p (Success rp)

export covering
checkDB : HasIO io => Env HasIdris -> EitherT PackErr io ()
checkDB e = do
  rep <- liftIO $ execStateT empty
                $ traverse_ (checkPkg e) (keys e.db.packages)
  putStrLn $ printReport rep
