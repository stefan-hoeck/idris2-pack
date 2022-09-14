module Pack.Core.IO

import public Control.Monad.Either
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

export
filterM : Monad m => (a -> m Bool) -> List a -> m (List a)
filterM f []        = pure []
filterM f (x :: xs) = do
  True <- f x | False => filterM f xs
  (x ::) <$> filterM f xs

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

||| Display a list of variable-value pairs in the format
||| `VAR1="val1" VAR2="val2"`.
export
dispEnv : List (String,String) -> String
dispEnv = unwords . map (\(e,v) => "\{e}=\{quote v}")

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
sysWithEnv = sys .: cmdWithEnv

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
sysRunWithEnv = sysRun .: cmdWithEnv

--------------------------------------------------------------------------------
--         Working with Directories
--------------------------------------------------------------------------------

||| Checks if a file at the given location exists.
export %inline
exists : HasIO io => (dir : Path Abs) -> io Bool
exists = exists . interpolate

||| Checks if a file at the given location exists.
export %inline
fileExists : HasIO io => (f : File Abs) -> io Bool
fileExists = exists . interpolate

||| Checks if a file at the given location is missing.
export
missing : HasIO io => (dir : Path Abs) -> io Bool
missing = map not . exists

||| Checks if a file at the given location is missing.
export
fileMissing : HasIO io => (f : File Abs) -> io Bool
fileMissing = map not . fileExists

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
  Just s <- currentDir | Nothing => throwE NoCurDir
  case the FilePath (fromString s) of
    FP (PAbs sx) => pure (PAbs sx)
    FP (PRel _)  => throwE NoCurDir

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
      -> (act : Path Abs -> EitherT PackErr io a)
      -> EitherT PackErr io a
inDir dir act =
  curDir >>= \cur => finally (chgDir cur) (chgDir dir >> act dir)

||| Returns the names of entries in a directory
export
entries :  HasIO io
        => (dir : Path Abs)
        -> EitherT PackErr io (List Body)
entries dir = do
  ss <- eitherIO (DirEntries dir) (listDir "\{dir}")
  pure (mapMaybe parse ss)

||| Returns the names of toml files in a directory
export
tomlFiles :  HasIO io
          => (dir : Path Abs)
          -> EitherT PackErr io (List Body)
tomlFiles dir = filter isTomlBody <$> entries dir

||| Returns the names of toml files in a directory
export
htmlFiles :  HasIO io
          => (dir : Path Abs)
          -> EitherT PackErr io (List Body)
htmlFiles dir = filter isHtmlBody <$> entries dir

||| Returns the names of entries in the current directory
export
currentEntries : HasIO io => EitherT PackErr io (List Body)
currentEntries = curDir >>= entries

||| Copy a directory.
export
copyDir : HasIO io => (from,to : Path Abs) -> EitherT PackErr io ()
copyDir from to = do
  mkParentDir to
  sys "cp -r \{from} \{to}"

||| Tries to fine a file, the body of which returns `True` for
||| the given prediccate.
export
findInParentDirs :  HasIO io
                 => (Body -> Bool)
                 -> Path Abs
                 -> EitherT PackErr io (Maybe (File Abs))
findInParentDirs p (PAbs sb) = go sb
  where go : SnocList Body -> EitherT PackErr io (Maybe (File Abs))
        go [<]       = pure Nothing
        go (sb :< b) =
          let dir := PAbs (sb :< b)
           in do
             (h :: _) <- filter p <$> entries dir | Nil => go sb
             pure $ Just (MkF dir h)

export
mkTmpDir : HasIO io => PackDir => EitherT PackErr io TmpDir
mkTmpDir = go 100 0
  where go : Nat -> Nat -> EitherT PackErr io TmpDir
        go 0     _ = throwE NoTmpDir
        go (S k) n =
          let Just body := Body.parse ".tmp\{show n}" | Nothing => go k (S n)
              dir       := packDir /> body
           in do
             False <- exists dir | True => go k (S n)
             mkDir dir
             pure (TD dir)

--------------------------------------------------------------------------------
--         File Access
--------------------------------------------------------------------------------

||| Delete a file.
export
rmFile : HasIO io => (f : File Abs) -> EitherT PackErr io ()
rmFile f = when !(fileExists f) $ sys "rm \{f}"

||| Tries to read the content of a file
export covering
read : HasIO io => File Abs -> EitherT PackErr io String
read fn = eitherIO (ReadFile fn) (readFile "\{fn}")

||| Reads the content of a file if it exists, otherwise
||| returns the given alternative string.
export covering
readIfExists :  HasIO io
             => (file : File Abs)
             -> (alt  : String)
             -> EitherT PackErr io String
readIfExists file alt = do
  True <- fileExists file | False => pure alt
  read file

||| Tries to write a string to a file.
||| The file's parent directory is created if
||| it does not yet exist.
export covering
write : HasIO io => File Abs -> String -> EitherT PackErr io ()
write file str = do
  mkDir file.parent
  eitherIO (WriteFile file) (writeFile "\{file}" str)

||| Creates a symbolic link from one path to another,
||| remove a link at path `to` if there already is one.
export
link : HasIO io => (from : Path Abs) -> (to : File Abs) -> EitherT PackErr io ()
link from to = do
  rmFile to
  mkDir to.parent
  sys "ln -s \{from} \{to}"

||| Copy a file.
export
copyFile : HasIO io => (from,to : File Abs) -> EitherT PackErr io ()
copyFile from to = do
  mkDir to.parent
  sys "cp \{from} \{to}"

||| Patch a file
export
patch :  HasIO io
      => (original : File Abs)
      -> (patch    : File Abs)
      -> EitherT PackErr io ()
patch o p = sys "patch \{o} \{p}"
