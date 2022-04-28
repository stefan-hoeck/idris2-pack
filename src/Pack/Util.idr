module Pack.Util

import public Control.Monad.Either
import public Pack.Types
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

export
sys : HasIO io => (cmd : String) -> EitherT PackErr io ()
sys cmd = do
  0 <- system cmd | n => throwE (Sys cmd n)
  pure ()

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
        => (env    : Env)
        => (url    : String)
        -> (commit : String)
        -> (act    : EitherT PackErr io a)
        -> EitherT PackErr io a
withGit url commit act = do
  cur <- curDir
  putStrLn "Cloning project"
  gitClone url env.packTmpDir
  putStrLn "Changing into repo directory"
  chgDir env.packTmpDir
  putStrLn "Checking out to \{commit}"
  gitCheckout commit
  res <- act
  chgDir cur
  putStrLn "Removing repo dir"
  sys "rm -rf \{env.packTmpDir}"
  pure res

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

export
packDir : HasIO io => EitherT PackErr io String
packDir = do
  Nothing <- getEnv "PACK_DIR" | Just v => pure v
  Nothing <- getEnv "HOME"     | Just v => pure "\{v}/.pack"
  throwE NoPackDir

covering
loadDB : HasIO io => String -> EitherT PackErr io DB
loadDB commit = do
  dir <- packDir
  let tmpDir = "\{dir}/tmp"
  cur <- curDir
  gitClone "https://github.com/stefan-hoeck/idris2-pack-db" tmpDir
  chgDir tmpDir
  gitCheckout commit
  str <- read "pack.db"
  case readDB str of
    Left err => do
      chgDir cur
      sys "rm -rf \{tmpDir}"
      throwE err
    Right res => do
      chgDir cur
      sys "rm -rf \{tmpDir}"
      pure res

export covering
env : HasIO io => (commit : String) -> EitherT PackErr io Env
env commit = do
  dir <- packDir
  mkDir dir

  db  <- loadDB commit

  let tmpDir   = "\{dir}/tmp"
      idrisDir = "\{dir}/\{db.idrisCommit}"

  mkDir idrisDir

  pure $ MkEnv db dir tmpDir idrisDir

export
run : EitherT PackErr IO () -> IO ()
run (MkEitherT io) = do
  Left err <- io | Right () => pure ()
  putStrLn (printErr err)
