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
getConfig' : HasIO io => EitherT PackErr io (Config Nothing,Cmd)
getConfig' = do
  dir        <- packDir
  db         <- readIfExists (dir /> ".db") "HEAD"
  pn :: args <- getArgs | Nil => pure (init dir $ MkDBName db, PrintHelp)
  (conf,cmd) <- liftEither $ applyArgs dir (MkDBName db) args
  let conf' : Config Nothing
      conf' = case cmd of
        SwitchRepo repo => {collection := repo} conf
        CheckDB    repo => {collection := repo} conf
        FromHEAD _      => {collection := "HEAD"} conf
        _               => conf
  pure (conf',cmd)

||| Read application config from command line arguments.
export covering
getConfig : HasIO io => EitherT PackErr io (Config Nothing,Cmd)
getConfig = getConfig' >>= \p@(c,_) => mkDir c.packDir $> p

||| Update the package database.
export
updateDB : HasIO io => Config s -> EitherT PackErr io ()
updateDB conf = do
  rmDir (dbDir conf)
  mkDir (dbDir conf)
  withGit (tmpDir conf) dbRepo "main" $ do
    sys "cp *.db \{show $ dbDir conf}"

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

covering
loadDB : HasIO io => (conf : Config s) -> EitherT PackErr io DB
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
env : HasIO io => Config s -> EitherT PackErr io (Env DBLoaded)
env conf = do
  db <- loadDB conf
  pure $ {db := db} conf
