module Pack.Admin.Runner.Check

import Control.Monad.State
import Data.SortedMap
import Idris.Package.Types
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
       -> SafeLib
       -> EitherT PackErr io ()
       -> StateT ReportDB io Report
report p rp act = do
  Right _ <- toState act | Left _ => updateRep p (Failure rp [])
  updateRep p (Success rp)

missing : ResolvedLib t -> ResolvedLib t
missing = {status := Missing}

covering
checkPkg :  HasIO io
         => Env HasIdris
         -> PkgName
         -> StateT ReportDB io Report
checkPkg e p = do
  Nothing  <- lookup p <$> get | Just rep => pure rep
  info e "Checking \{p}"
  Right rl <- toState $ resolveLib e p >>= safeLib e . missing
    | Left err => warn e "Could not resolve \{p}" >>
                  updateRep p (Error p err)
  [] <- failingDeps <$> traverse (checkPkg e) (dependencies rl)
    | rs => warn e "\{p} had failing dependencies" >>
            updateRep p (Failure rl rs)
  Right () <- toState $ installAny e $ Left rl
    | Left err => warn e "Failed to build \{p}" >>
                  updateRep p (Error p err)
  updateRep p (Success rl)

copyDocs :  HasIO io
         => File Abs
         -> Env HasIdris
         -> ReportDB
         -> EitherT PackErr io ()
copyDocs f e db = traverse_ go db
  where go : Report -> EitherT PackErr io ()
        go (Success rl) = do
            installDocs e rl
            copyDir (pkgDocs e rl.name rl.pkg)
                    (f.parent /> "docs" <//> name rl)
        go _            = pure ()

export covering
checkDB : HasIO io => File Abs -> Env HasIdris -> EitherT PackErr io ()
checkDB p e =
  let ps := map (MkPkgName . interpolate) corePkgs
         ++ keys e.db.packages
   in do
     install e [(Bin, "katla")]
     rep <- liftIO $ execStateT empty
                   $ traverse_ (checkPkg e) ps
     write p (printReport e rep)
     copyDocs p e rep
     case numberOfFailures rep of
       0 => pure ()
       n => throwE (BuildFailures n)
