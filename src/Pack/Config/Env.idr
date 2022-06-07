module Pack.Config.Env

import Data.Maybe
import Data.String
import Libraries.Utils.Path
import Pack.CmdLn.Opts
import Pack.CmdLn.Types
import Pack.Config.TOML
import Pack.Config.Types
import Pack.Core
import Pack.Core.TOML
import Pack.Database.TOML
import Pack.Database.Types
import System

%default total

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

export
log :  HasIO io
    => (conf : Config s)
    -> (lvl  : LogLevel)
    -> (msg  : Lazy String)
    -> io ()
log c lvl msg = when (lvl >= c.logLevel) (putStrLn "[ \{lvl} ] \{msg}")

export
debug : HasIO io => (conf : Config s) -> (msg  : Lazy String) -> io ()
debug c = log c Debug

export
info : HasIO io => (conf : Config s) -> (msg  : Lazy String) -> io ()
info c = log c Info

export
warn : HasIO io => (conf : Config s) -> (msg  : Lazy String) -> io ()
warn c = log c Warning

configPath : Path -> Path
configPath dir = dir /> "user" /> "pack.toml"

||| Return the path of the *pack* root directory,
||| either from environment variable `$PACK_DIR`, or
||| as `$HOME/.pack`.
export
packDir : HasIO io => EitherT PackErr io Path
packDir = do
  Nothing <- getEnv "PACK_DIR" | Just v => pure (parse v)
  Nothing <- getEnv "HOME"     | Just v => pure (parse v /> ".pack")
  throwE NoPackDir

||| Update the package database.
export
updateDB_ : HasIO io => (packDir : Path) -> EitherT PackErr io ()
updateDB_ packDir = do
  rmDir (dbDir_ packDir)
  withGit (tmpDir_ packDir) dbRepo "main" $
    copyDir (tmpDir_ packDir /> "collections") (dbDir_ packDir)

||| Loads the name of the default collection (currently the latest
||| nightly)
export
defaultColl : HasIO io => (packDir : Path) -> EitherT PackErr io DBName
defaultColl packDir = do
  when !(missing $ dbDir_ packDir) (updateDB_ packDir)
  (x :: xs) <- filter ("HEAD.toml" /=) <$> tomlFiles (dbDir_ packDir)
    | [] => pure $ MkDBName "HEAD"
  pure . MkDBName . fromMaybe "HEAD" . fileStem $ foldl max x xs

||| Update the package database.
export
updateDB : HasIO io => Config s -> EitherT PackErr io ()
updateDB conf = do
  debug conf "updating data collections"
  updateDB_ conf.packDir

||| Read application config from command line arguments.
export covering
getConfig :  HasIO io
          => (readCmd : List String -> Either PackErr a)
          -> (dflt    : a)
          -> EitherT PackErr io (Config Nothing,a)
getConfig readCmd dflt = do
  dir        <- packDir
  coll       <- defaultColl dir

  -- Initialize `pack.toml` if none exists
  when !(missing $ configPath dir) $
    write (configPath dir) (initToml "scheme" coll)

  global     <- readOptionalFromTOML (configPath dir) config
  local      <- readOptionalFromTOML (parse "pack.toml") config

  let ini = init dir coll `update` global `update` local

  pn :: args <- getArgs | Nil => pure (ini, dflt)
  (conf,cmd) <- liftEither $ applyArgs ini args readCmd
  debug conf "Config loaded"
  mkDir conf.packDir
  pure (conf,cmd)

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

covering
loadDB : HasIO io => (conf : Config s) -> EitherT PackErr io DB
loadDB conf = do
  when !(missing $ dbDir conf) (updateDB conf)
  debug conf "reading package collection"
  readFromTOML (dbFile conf) (fromTOML)

||| Load the package database and create a package
||| environment.
export covering
env : HasIO io => Config s -> EitherT PackErr io (Env DBLoaded)
env conf = do
  db <- loadDB conf
  pure $ {db := db} conf
