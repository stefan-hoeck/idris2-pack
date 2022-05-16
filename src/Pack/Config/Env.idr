module Pack.Config.Env

import Data.String
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
log c lvl msg = when (lvl >= c.logLevel) (putStrLn "[ \{show lvl} ] \{msg}")

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

||| Read application config from command line arguments.
export covering
getConfig :  HasIO io
          => (readCmd : List String -> Either PackErr a)
          -> (dflt    : a)
          -> EitherT PackErr io (Config Nothing,a)
getConfig readCmd dflt = do
  dir        <- packDir
  ini        <- readFromTOML (configPath dir) (config dir)
  pn :: args <- getArgs | Nil => pure (ini, dflt)
  (conf,cmd) <- liftEither $ applyArgs ini args readCmd
  debug conf "Config loaded"
  mkDir conf.packDir
  pure (conf,cmd)

||| Update the package database.
export
updateDB : HasIO io => Config s -> EitherT PackErr io ()
updateDB conf = do
  debug conf "removing db dir"
  rmDir (dbDir conf)
  withGit (tmpDir conf) dbRepo "main" $ do
    debug conf "copying data collections"
    copyDir (tmpDir conf /> "collections") (dbDir conf)

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

covering
loadDB : HasIO io => (conf : Config s) -> EitherT PackErr io DB
loadDB conf = do
  dbDirExists <- exists (dbDir conf)
  when (not dbDirExists) (updateDB conf)
  debug conf "reading package collection"
  readFromTOML (dbFile conf) (fromTOML)

||| Load the package database and create a package
||| environment.
export covering
env : HasIO io => Config s -> EitherT PackErr io (Env DBLoaded)
env conf = do
  db <- loadDB conf
  pure $ {db := db} conf
