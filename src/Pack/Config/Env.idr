module Pack.Config.Env

import Data.Maybe
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

configPath : Path Abs -> File Abs
configPath dir = MkF (dir /> "user") packToml

getEnvPath : HasIO io => String -> io (Maybe (Path Abs))
getEnvPath s = (>>= parse) <$> getEnv s

||| Return the path of the *pack* root directory,
||| either from environment variable `$PACK_DIR`, or
||| as `$HOME/.pack`.
export
packDir : HasIO io => EitherT PackErr io (Path Abs)
packDir = do
  Nothing <- getEnvPath "PACK_DIR" | Just v => pure v
  Nothing <- getEnvPath "HOME"     | Just v => pure (v /> ".pack")
  throwE NoPackDir

||| Update the package database.
export
updateDB_ : HasIO io => (packDir : Path Abs) -> EitherT PackErr io ()
updateDB_ packDir = do
  rmDir (dbDir_ packDir)
  withGit (tmpDir_ packDir) dbRepo "main" $ \d =>
    copyDir (d /> "collections") (dbDir_ packDir)

||| Loads the name of the default collection (currently the latest
||| nightly)
export
defaultColl : HasIO io => (packDir : Path Abs) -> EitherT PackErr io DBName
defaultColl packDir = do
  when !(missing $ dbDir_ packDir) (updateDB_ packDir)
  (x :: xs) <- filter ("HEAD.toml" /=) <$> tomlFiles (dbDir_ packDir)
    | [] => pure Head
  pure
    . maybe Head MkDBName
    . fileStem
    $ foldl max x xs

||| Update the package database.
export
updateDB : HasIO io => Config s -> EitherT PackErr io ()
updateDB conf = do
  debug conf "updating data collections"
  updateDB_ conf.packDir

resolveMeta : HasIO io => UserPackage -> EitherT PackErr io Package
resolveMeta (GitHub u (MC x) i p) = pure $ GitHub u x i p
resolveMeta (GitHub u (Latest x) i p) =
  map (\c => GitHub u c i p) $ gitLatest u (MkCommit x)
resolveMeta (Local d i p) = pure $ Local d i p

||| Read application config from command line arguments.
export covering
getConfig :  HasIO io
          => (readCmd   : Path Abs -> List String -> Either PackErr a)
          -> (dflt      : a)
          -> (dfltLevel : a -> LogLevel)
          -> EitherT PackErr io (Config Nothing,a)
getConfig readCmd dflt dfltLevel = do
  -- relevant directories
  cur        <- curDir
  dir        <- packDir
  coll       <- defaultColl dir

  let globalConfig       = configPath dir

  -- Initialize `pack.toml` if none exists
  when !(fileMissing globalConfig) $
    write globalConfig (initToml "scheme" coll)

  localToml   <- findInParentDirs ("pack.toml" ==) cur
  global'     <- readOptionalFromTOML globalConfig config
  local'      <- case localToml of
    Just af => readFromTOML af config
    Nothing => readOptionalFromTOML (MkF cur packToml) config
  global      <- traverse resolveMeta global'
  local       <- traverse resolveMeta local'

  let ini = init cur dir coll `update` global `update` local

  pn :: args <- getArgs | Nil => pure (ini, dflt)
  (conf,cmd) <- liftEither $ applyArgs cur ini args (readCmd cur) dfltLevel

  debug conf "Pack home is \{dir}"
  debug conf "Current directory is \{cur}"
  case localToml of
    Just af => info conf "Found local config at \{af}"
    Nothing => debug conf "No local config found"
  info conf "Using package collection \{conf.collection}"
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
  readFromTOML (dbFile conf) db

||| Load the package database and create a package
||| environment.
export covering
env : HasIO io => Config s -> EitherT PackErr io (Env DBLoaded)
env conf = do
  db <- loadDB conf
  pure $ {db := db} conf

adjCollection : DBName -> String -> String
adjCollection db str = case isPrefixOf "collection " str of
  False => str
  True  => "collection = \"\{db}\""

export covering
writeCollection : HasIO io => Config s -> EitherT PackErr io ()
writeCollection c =
  let toml = configPath c.packDir
   in do
     str <- read toml
     write toml (unlines . map (adjCollection c.collection) $ lines str)
