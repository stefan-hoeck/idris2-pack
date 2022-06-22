module Pack.Core.IO

import public Control.Monad.Either
import Data.FilePath
import Pack.Core.Types
import System
import System.Directory
import System.File

%default total

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

export
mapMaybeM : Monad m => (a -> m (Maybe b)) -> List a -> m (List b)
mapMaybeM f []        = pure []
mapMaybeM f (x :: xs) = do
  Just vb <- f x | Nothing => mapMaybeM f xs
  (vb ::) <$> mapMaybeM f xs


||| Convert an IO action with the potential of failure
||| to an `EitherT PackErr`.
export
eitherIO :  HasIO io
         => (toErr : err -> PackErr)
         -> (act : io (Either err a))
         -> EitherT PackErr io a
eitherIO toErr = MkEitherT . map (mapFst toErr)

||| Make sure a *cleanup* action is run after
||| an IO action that might fail.
export
finally :  Monad m
        => (cleanup : EitherT err m ())
        -> (act     : EitherT err m a)
        -> EitherT err m a
finally cleanup act = MkEitherT $ do
  res <- runEitherT act
  ignore $ runEitherT cleanup
  pure res

||| Runs a *pack* program, printing errors to standard out.
export
run : EitherT PackErr IO () -> IO ()
run (MkEitherT io) = do
  Left err <- io | Right () => pure ()
  die (printErr err)

--------------------------------------------------------------------------------
--          System Commands
--------------------------------------------------------------------------------

export
dispEnv : List (String,String) -> String
dispEnv = unwords . map (\(e,v) => "\{e}=\"\{v}\"")

||| Tries to run a system command.
export
sys : HasIO io => (cmd : String) -> EitherT PackErr io ()
sys cmd = do
  0 <- system cmd | n => throwE (Sys cmd n)
  pure ()

cmdWithEnv : String -> List (String,String) -> String
cmdWithEnv cmd []  = cmd
cmdWithEnv cmd env = "\{dispEnv env} \{cmd}"

||| Tries to run a system command prefixed with the given
||| environment variables.
|||
||| Note: In case of an error, the environment will not be part
||| of the command listed in the error message. This is a
||| deliberate choice to declutter Idris output in case of a
||| failed build. If the environment should be included in the
||| error message, just prefix `cmd` accordingly and use `sys`.
export
sysWithEnv :  HasIO io
           => (cmd : String)
           -> (env : List (String,String))
           -> EitherT PackErr io ()
sysWithEnv cmd env = do
  0 <- system (cmdWithEnv cmd env) | n => throwE (Sys cmd n)
  pure ()

||| Tries to run a system command returning its output.
export covering
sysRun : HasIO io => (cmd : String) -> EitherT PackErr io String
sysRun cmd = do
  (res,0) <- System.run cmd | (_,n) => throwE (Sys cmd n)
  pure res

||| Tries to run a system command prefixed with the given
||| environment variables returning its output.
|||
||| Note: In case of an error, the environment will not be part
||| of the command listed in the error message. This is a
||| deliberate choice to declutter Idris output in case of a
||| failed build. If the environment should be included in the
||| error message, just prefix `cmd` accordingly and use `sys`.
export covering
sysRunWithEnv :  HasIO io
              => (cmd : String)
              -> (env : List (String,String))
              -> EitherT PackErr io String
sysRunWithEnv cmd env = do
  (res,0) <- System.run (cmdWithEnv cmd env) | (_,n) => throwE (Sys cmd n)
  pure res

--------------------------------------------------------------------------------
--         Working with Directories
--------------------------------------------------------------------------------

||| Checks if a file at the given location exists.
export
exists : HasIO io => (dir : Path Abs) -> io Bool
exists = exists . interpolate

||| Checks if a file at the given location is missing.
export
missing : HasIO io => (dir : Path Abs) -> io Bool
missing = map not . exists

||| Tries to create a director (including parent directories)
export
mkDir : HasIO io => (dir : Path Abs) -> EitherT PackErr io ()
mkDir (PAbs [<]) = pure ()
mkDir d          = sys "mkdir -p \{d}"

||| Creates a parent directory of a (file) path
export
mkParentDir : HasIO io => (p : Path Abs) -> EitherT PackErr io ()
mkParentDir p = whenJust (parentDir p) mkDir

||| Forcefully deletes a directory with all its content
export
rmDir : HasIO io => (dir : Path Abs) -> EitherT PackErr io ()
rmDir dir = when !(exists dir) $ sys "rm -rf \{dir}"

||| Returns the current directory's path.
export
curDir : HasIO io => EitherT PackErr io (Path Abs)
curDir = do
  Just s <- currentDir | Nothing => throwE CurDir
  case the FilePath (fromString s) of
    FP (PAbs sx) => pure (PAbs sx)
    FP (PRel _)  => throwE CurDir

||| Changes the working directory
export
chgDir : HasIO io => (dir : Path Abs) -> EitherT PackErr io ()
chgDir dir = do
  True <- changeDir "\{dir}" | False => throwE (ChangeDir dir)
  pure ()

||| Runs an action in the given directory, changing back
||| to the current directory afterwards.
export
inDir :  HasIO io
      => (dir : Path Abs)
      -> (act : EitherT PackErr io a)
      -> EitherT PackErr io a
inDir dir act =
  curDir >>= \cur => finally (chgDir cur) (chgDir dir >> act)

||| Returns the names of entries in a directory
export
entries :  HasIO io
        => (dir : Path Abs)
        -> EitherT PackErr io (List $ Path Rel)
entries dir = do
  ss <- eitherIO (DirEntries dir) (listDir "\{dir}")
  pure (map (neutral />) $ mapMaybe body ss)

||| Returns the names of entries in a directory
export
tomlFiles :  HasIO io
          => (dir : Path Abs)
          -> EitherT PackErr io (List $ Path Rel)
tomlFiles dir = filter isTomlFile <$> entries dir

||| Returns the names of entries in the current directory
export
currentEntries : HasIO io => EitherT PackErr io (List $ Path Rel)
currentEntries = curDir >>= entries

||| Copy a directory.
export
copyDir : HasIO io => (from,to : Path Abs) -> EitherT PackErr io ()
copyDir from to = do
  mkParentDir to
  sys "cp -r \{from} \{to}"

--------------------------------------------------------------------------------
--         File Access
--------------------------------------------------------------------------------

||| Delete a file.
export
rmFile : HasIO io => (f : Path Abs) -> EitherT PackErr io ()
rmFile f = when !(exists f) $ sys "rm \{f}"

||| Tries to read the content of a file
export covering
read : HasIO io => Path Abs -> EitherT PackErr io String
read fn = eitherIO (ReadFile fn) (readFile "\{fn}")

||| Reads the content of a file if it exists, otherwise
||| returns the given alternative string.
export covering
readIfExists :  HasIO io
             => (path : Path Abs)
             -> (alt  : String)
             -> EitherT PackErr io String
readIfExists path alt = do
  True <- exists path | False => pure alt
  read path

||| Tries to write a string to a file.
||| The file's parent directory is created if
||| it does not yet exist.
export covering
write : HasIO io => Path Abs -> String -> EitherT PackErr io ()
write path str = do
  mkParentDir path
  eitherIO (WriteFile path) (writeFile "\{path}" str)

||| Creates a symbolic link from one path to another,
||| remove a link at path `to` if there already is one.
export
link : HasIO io => (from,to : Path Abs) -> EitherT PackErr io ()
link from to = do
  rmFile to
  mkParentDir to
  sys "ln -s \{from} \{to}"

||| Copy a file.
export
copyFile : HasIO io => (from,to : Path Abs) -> EitherT PackErr io ()
copyFile from to = do
  mkParentDir to
  sys "cp \{from} \{to}"

||| Patch a file
export
patch :  HasIO io
      => (original : Path Abs)
      -> (patch    : Path Abs)
      -> EitherT PackErr io ()
patch o p = do sys "patch \{o} \{p}"
