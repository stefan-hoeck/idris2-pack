module Pack.Util

import public Control.Monad.Either
import Pack.CmdLn
import Pack.Err
import Pack.Types
import System.Directory
import System

%default total

export
eitherIO :  HasIO io
         => (toErr : err -> PackErr)
         -> (act : io (Either err a))
         -> EitherT PackErr io a
eitherIO toErr = MkEitherT . map (mapFst toErr)

export covering
read : HasIO io => String -> EitherT PackErr io String
read fn = eitherIO (ReadFile fn) (readFile fn)

export covering
write : HasIO io => String -> String -> EitherT PackErr io ()
write path str = eitherIO (WriteFile path) (writeFile path str)

export covering
readIfExists :  HasIO io
             => (path : String)
             -> (alt  : String)
             -> EitherT PackErr io String
readIfExists path alt = do
  True <- exists path | False => pure alt
  read path


export
sys : HasIO io => (cmd : String) -> EitherT PackErr io ()
sys cmd = do
  0 <- system cmd | n => throwE (Sys cmd n)
  pure ()

export
rmFile : HasIO io => (f : String) -> EitherT PackErr io ()
rmFile f = when !(exists f) $ sys "rm \{f}"

export
rmDir : HasIO io => (dir : String) -> EitherT PackErr io ()
rmDir dir = when !(exists dir) $ sys "rm -rf \{dir}"

export
curDir : HasIO io => EitherT PackErr io String
curDir = do
  Just s <- currentDir | Nothing => throwE CurDir
  pure s

export
chgDir : HasIO io => (dir : String) -> EitherT PackErr io ()
chgDir dir = do
  True <- changeDir dir | False => throwE (ChangeDir dir)
  pure ()

export
mkDir : HasIO io => (path : String) -> EitherT PackErr io ()
mkDir path = do
  False <- liftIO (exists path) | True => pure ()
  eitherIO (MkDir path) (createDir path)

--------------------------------------------------------------------------------
--          Git
--------------------------------------------------------------------------------

export
gitClone :  HasIO io
         => (url : String)
         -> (dest : String)
         -> EitherT PackErr io ()
gitClone url dest = sys "git clone \{url} \{dest}"

export
gitCheckout : HasIO io => (commit : String) -> EitherT PackErr io ()
gitCheckout commit = sys "git checkout \{commit}"

export
withGit :  HasIO io
        => (conf   : Config)
        -> (url    : String)
        -> (commit : String)
        -> (act    : EitherT PackErr io a)
        -> EitherT PackErr io a
withGit conf url commit act = do
  cur <- curDir
  rmDir (tmpDir conf)
  gitClone url (tmpDir conf)
  chgDir (tmpDir conf)
  gitCheckout commit
  res <- act
  chgDir cur
  rmDir (tmpDir conf)
  pure res

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

||| Return the path of the *pack* root directory,
||| either from environment variable `$PACK_DIR`, or
||| as `$HOME/.pack`.
export
packDir : HasIO io => EitherT PackErr io String
packDir = do
  Nothing <- getEnv "PACK_DIR" | Just v => pure v
  Nothing <- getEnv "HOME"     | Just v => pure "\{v}/.pack"
  throwE NoPackDir

covering
getConfig' : HasIO io => EitherT PackErr io Config
getConfig' = do
  dir        <- packDir
  db         <- readIfExists "\{dir}/.db" "HEAD"
  pn :: args <- getArgs | Nil => pure (init dir db)
  conf       <- liftEither $ applyArgs dir db args
  case conf.cmd of
    SwitchRepo repo => pure $ {dbVersion := repo} conf
    CheckDB    repo => pure $ {dbVersion := repo} conf
    _               => pure conf


||| Read application config from command line arguments.
export covering
getConfig : HasIO io => EitherT PackErr io Config
getConfig = do
  c <- getConfig'
  mkDir c.packDir
  pure c

||| Where the package database is stored.
|||
||| TODO: This should become a command line argument
export
dbRepo : String
dbRepo = "https://github.com/stefan-hoeck/idris2-pack-db"

||| Update the package database.
export
updateDB : HasIO io => Config -> EitherT PackErr io ()
updateDB conf = do 
  rmDir (dbDir conf)
  mkDir (dbDir conf)
  withGit conf dbRepo "HEAD" $ do
    sys "cp *.db \{dbDir conf}"

covering
loadDB : HasIO io => (conf : Config) -> EitherT PackErr io DB
loadDB conf = do
  dbDirExists <- exists (dbDir conf)
  when (not dbDirExists) (updateDB conf)
  str <- read "\{dbDir conf}/\{conf.dbVersion}.db"
  case readDB str of
    Left err  => throwE err
    Right res => pure res

export covering
env : HasIO io => Config -> EitherT PackErr io Env
env conf = do
  db <- loadDB conf
  pure $ MkEnv db conf

||| Runs a *pack* program, printing errors to standard out.
export
run : EitherT PackErr IO () -> IO ()
run (MkEitherT io) = do
  Left err <- io | Right () => pure ()
  putStrLn (printErr err)
