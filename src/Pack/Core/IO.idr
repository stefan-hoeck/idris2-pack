module Pack.Core.IO

import public Control.Monad.Either
import public Libraries.Utils.Path
import Pack.Core.Types
import System
import System.Directory
import System.File

%default total

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

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
  putStrLn (printErr err)

--------------------------------------------------------------------------------
--          System Commands
--------------------------------------------------------------------------------

||| Tries to run a system command.
export
sys : HasIO io => (cmd : String) -> EitherT PackErr io ()
sys cmd = do
  0 <- system cmd | n => throwE (Sys cmd n)
  pure ()

||| Tries to run a system command returning its output.
export covering
sysRun : HasIO io => (cmd : String) -> EitherT PackErr io String
sysRun cmd = do
  (res,0) <- System.run cmd | (_,n) => throwE (Sys cmd n)
  pure res

--------------------------------------------------------------------------------
--         Working with Directories
--------------------------------------------------------------------------------

||| Checks if a file at the given location exists.
export
exists : HasIO io => (dir : Path) -> io Bool
exists = exists . show

||| Checks if a file at the given location is missing.
export
missing : HasIO io => (dir : Path) -> io Bool
missing = map not . exists

||| Tries to create a director (including parent directories)
export
mkDir : HasIO io => (dir : Path) -> EitherT PackErr io ()
mkDir dir = case show dir of
  "" => pure ()
  s  => sys "mkdir -p \{s}"

||| Creates a parent directory of a (file) path
export
mkParentDir : HasIO io => (dir : Path) -> EitherT PackErr io ()
mkParentDir dir = whenJust (parent $ show dir) (mkDir . parse)

||| Forcefully deletes a directory with all its content
export
rmDir : HasIO io => (dir : Path) -> EitherT PackErr io ()
rmDir dir = when !(exists dir) $ sys "rm -rf \{show dir}"

||| Returns the current directory's path.
export
curDir : HasIO io => EitherT PackErr io Path
curDir = do
  Just s <- currentDir | Nothing => throwE CurDir
  pure $ parse s

||| Changes the working directory
export
chgDir : HasIO io => (dir : Path) -> EitherT PackErr io ()
chgDir dir = do
  True <- changeDir (show dir) | False => throwE (ChangeDir dir)
  pure ()

||| Runs an action in the given directory, changing back
||| to the current directory afterwards.
export
inDir :  HasIO io
      => (dir : Path)
      -> (act : EitherT PackErr io a)
      -> EitherT PackErr io a
inDir dir act =
  curDir >>= \cur => finally (chgDir cur) (chgDir dir >> act)

||| Returns the names of entries in a directory
export
entries : HasIO io => (dir : Path) -> EitherT PackErr io (List String)
entries dir = eitherIO (DirEntries dir) (listDir $ show dir)

||| Returns the names of entries in a directory
export
tomlFiles : HasIO io => (dir : Path) -> EitherT PackErr io (List String)
tomlFiles dir = filter ((Just "toml" ==) . extension) <$> entries dir

||| Returns the names of entries in the current directory
export
currentEntries : HasIO io => EitherT PackErr io (List String)
currentEntries = entries (parse ".")

||| Copy a directory.
export
copyDir : HasIO io => (from,to : Path) -> EitherT PackErr io ()
copyDir from to = do
  mkParentDir to
  sys "cp -r \{show from} \{show to}"

--------------------------------------------------------------------------------
--         File Access
--------------------------------------------------------------------------------

||| Delete a file.
export
rmFile : HasIO io => (f : Path) -> EitherT PackErr io ()
rmFile f = when !(exists f) $ sys "rm \{show f}"

||| Tries to read the content of a file
export covering
read : HasIO io => Path -> EitherT PackErr io String
read fn = eitherIO (ReadFile fn) (readFile $ show fn)

||| Reads the content of a file if it exists, otherwise
||| returns the given alternative string.
export covering
readIfExists :  HasIO io
             => (path : Path)
             -> (alt  : String)
             -> EitherT PackErr io String
readIfExists path alt = do
  True <- exists path | False => pure alt
  read path

||| Tries to write a string to a file.
||| The file's parent directory is created if
||| it does not yet exist.
export covering
write : HasIO io => Path -> String -> EitherT PackErr io ()
write path str = do
  mkParentDir path
  eitherIO (WriteFile path) (writeFile (show path) str)

||| Creates a symbolic link from one path to another,
||| remove a link at path `to` if there already is one.
export
link : HasIO io => (from,to : Path) -> EitherT PackErr io ()
link from to = do
  rmFile to
  mkParentDir to
  sys "ln -s \{show from} \{show to}"

||| Copy a file.
export
copyFile : HasIO io => (from,to : Path) -> EitherT PackErr io ()
copyFile from to = do
  mkParentDir to
  sys "cp \{show from} \{show to}"

||| Patch a file
export
patch :  HasIO io
      => (original : Path)
      -> (patch    : Path)
      -> EitherT PackErr io ()
patch o p = do sys "patch \{show o} \{show p}"
