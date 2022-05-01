module Pack.Util

import public Control.Monad.Either
import Data.String
import Libraries.Data.SortedMap
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

export
finally :  Monad m
        => EitherT err m ()
        -> EitherT err m a
        -> EitherT err m a
finally cleanup act = MkEitherT $ do
  res <- runEitherT act
  ignore $ runEitherT cleanup
  pure res


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

export covering
sysRun : HasIO io => (cmd : String) -> EitherT PackErr io String
sysRun cmd = do
  (res,0) <- System.run cmd | (_,n) => throwE (Sys cmd n)
  pure res

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

||| Runs a program in the given directory, changing back
||| to the current directory afterwards.
export
inDir :  HasIO io
      => (dir : String)
      -> EitherT PackErr io a
      -> EitherT PackErr io a
inDir dir act = do
  cur <- curDir
  finally (chgDir cur) $ chgDir dir >> act

export
mkDir : HasIO io => (path : String) -> EitherT PackErr io ()
mkDir path = do
  False <- liftIO (exists path) | True => pure ()
  eitherIO (MkDir path) (createDir path)

--------------------------------------------------------------------------------
--          Git
--------------------------------------------------------------------------------

export
idrisRepo : String
idrisRepo = "https://github.com/idris-lang/Idris2.git"

export
gitClone :  HasIO io
         => (url : String)
         -> (dest : String)
         -> EitherT PackErr io ()
gitClone url dest = sys "git clone \{url} \{dest}"

export
gitCheckout : HasIO io => (commit : String) -> EitherT PackErr io ()
gitCheckout commit = sys "git checkout \{commit}"

export covering
gitLatest : HasIO io => (url : String) -> EitherT PackErr io String
gitLatest url = fst . break isSpace <$> sysRun "git ls-remote \{url} main"

export
withGit :  HasIO io
        => (conf   : Config)
        -> (url    : String)
        -> (commit : String)
        -> (act    : EitherT PackErr io a)
        -> EitherT PackErr io a
withGit conf url commit act =
  finally (rmDir (tmpDir conf)) $ do
    gitClone url (tmpDir conf)
    inDir (tmpDir conf) (gitCheckout commit >> act)

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
    FromHEAD _      => pure $ {dbVersion := "HEAD"} conf
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
  str  <- read "\{dbFile conf}"
  loc  <- readIfExists "\{userDB conf}" ""
  glob <- readIfExists "\{userGlobalDB conf}" ""
  case readDB (unlines [str,glob,loc]) of
    Left err  => throwE err
    Right res => pure res

export covering
env : HasIO io => Config -> EitherT PackErr io Env
env conf = do
  db <- loadDB conf
  pure $ MkEnv db conf

covering
latestForHEAD : HasIO io => Package -> EitherT PackErr io Package
latestForHEAD (MkPackage n url (Just "HEAD") ipkg) = do
  commit <- gitLatest url
  pure $ MkPackage n url (Just commit) ipkg
latestForHEAD p                                    = pure p

export covering
latestDBForHead : HasIO io => DB -> EitherT PackErr io DB
latestDBForHead (MkDB commit v ps) = do
  nc  <- gitLatest idrisRepo
  nps <- traverse latestForHEAD ps
  pure $ MkDB nc v nps

export covering
fromHEAD : HasIO io => String -> Env -> EitherT PackErr io ()
fromHEAD path (MkEnv db _) = do
  ndb <- latestDBForHead db
  write path (printDB ndb)

||| Runs a *pack* program, printing errors to standard out.
export
run : EitherT PackErr IO () -> IO ()
run (MkEitherT io) = do
  Left err <- io | Right () => pure ()
  putStrLn (printErr err)
