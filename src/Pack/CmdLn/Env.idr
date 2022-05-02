module Pack.CmdLn.Env

import Data.String
import Pack.CmdLn.Opts
import Pack.CmdLn.Types
import Pack.Core
import Pack.Database.Types
import System

%default total

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

||| Return the path of the *pack* root directory,
||| either from environment variable `$PACK_DIR`, or
||| as `$HOME/.pack`.
export
packDir : HasIO io => EitherT PackErr io Path
packDir = do
  Nothing <- getEnv "PACK_DIR" | Just v => pure (parse v)
  Nothing <- getEnv "HOME"     | Just v => pure (parse v /> ".pack")
  throwE NoPackDir

covering
getConfig' : HasIO io => EitherT PackErr io Config
getConfig' = do
  dir        <- packDir
  db         <- readIfExists (dir /> ".db") "HEAD"
  pn :: args <- getArgs | Nil => pure (init dir $ MkDBName db)
  conf       <- liftEither $ applyArgs dir (MkDBName db) args
  case conf.cmd of
    SwitchRepo repo => pure $ {dbVersion := repo} conf
    CheckDB    repo => pure $ {dbVersion := repo} conf
    FromHEAD _      => pure $ {dbVersion := "HEAD"} conf
    _               => pure conf

||| Read application config from command line arguments.
export covering
getConfig : HasIO io => EitherT PackErr io Config
getConfig = getConfig' >>= \c => mkDir c.packDir $> c

||| Update the package database.
export
updateDB : HasIO io => Config -> EitherT PackErr io ()
updateDB conf = do 
  rmDir (dbDir conf)
  mkDir (dbDir conf)
  withGit (tmpDir conf) dbRepo "main" $ do
    sys "cp *.db \{show $ dbDir conf}"

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

covering
loadDB : HasIO io => (conf : Config) -> EitherT PackErr io DB
loadDB conf = do
  dbDirExists <- exists (dbDir conf)
  when (not dbDirExists) (updateDB conf)
  str  <- read (dbFile conf)
  loc  <- readIfExists (userDB conf) ""
  glob <- readIfExists (userGlobalDB conf) ""
  case readDB (unlines [str,glob,loc]) of
    Left err  => throwE err
    Right res => pure res

||| Load the package database and create package
||| environment.
export covering
env : HasIO io => Config -> EitherT PackErr io (Env None)
env conf = do
  db <- loadDB conf
  pure $ MkEnv db conf
