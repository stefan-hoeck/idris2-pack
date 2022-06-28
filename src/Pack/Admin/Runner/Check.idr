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
  Right rp <- toState $ resolve e p
    | Left err => updateRep p (Error p err)
  [] <- failingDeps <$> traverse (checkPkg e) (dependencies rp)
    | rs => updateRep p (Failure rp rs)
  Right () <- toState $ installLib e p
    | Left err => updateRep p (Error p err)
  updateRep p (Success rp)

export covering
checkDB : HasIO io => File Abs -> Env HasIdris -> EitherT PackErr io ()
checkDB p e = do
  rep <- liftIO $ execStateT empty
                $ traverse_ (checkPkg e) (keys e.db.packages)
  write p (printReport e rep)
  case numberOfFailures rep of
    0 => pure ()
    n => throwE (BuildFailures n)
