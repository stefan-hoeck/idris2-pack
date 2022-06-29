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
         -> (PkgType,PkgName)
         -> StateT ReportDB io Report
checkPkg e (t,p) = do
  Nothing  <- lookup p <$> get | Just rep => pure rep
  info e "Checking \{p}"
  Right rp <- toState $ resolve e p
    | Left err => updateRep p (Error p err)
  [] <- failingDeps <$> traverse (checkPkg e) (map (Lib,) $ dependencies rp)
    | rs => updateRep p (Failure rp rs)
  case t of
    Lib => do
      Right () <- toState $ installLib e rp
        | Left err => updateRep p (Error p err)
      updateRep p (Success rp)
    Bin => do
      Right () <- toState $ installApp e rp
        | Left err => updateRep p (Error p err)
      updateRep p (Success rp)

copyDocs :  HasIO io
         => File Abs
         -> Env HasIdris
         -> ReportDB
         -> EitherT PackErr io ()
copyDocs f e db = traverse_ go db
  where go : Report -> EitherT PackErr io ()
        go (Success rp) = do
          installDocs e rp
          copyDir (packageDocs e rp) (f.parent /> "docs" <//> name rp)
        go _            = pure ()

export covering
checkDB : HasIO io => File Abs -> Env HasIdris -> EitherT PackErr io ()
checkDB p e =
  let ps := (Bin, "katla") :: map (Lib,) (keys e.db.packages)
   in do
     rep <- liftIO $ execStateT empty
                   $ traverse_ (checkPkg e) ps
     write p (printReport e rep)
     copyDocs p e rep
     case numberOfFailures rep of
       0 => pure ()
       n => throwE (BuildFailures n)
