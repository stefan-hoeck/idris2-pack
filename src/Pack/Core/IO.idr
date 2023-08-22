module Pack.Core.IO

import public Control.Monad.Either
import Pack.Config.Types
import Pack.Core.Logging
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
eitherIO :
     {auto _ : HasIO io}
  -> (toErr : err -> PackErr)
  -> (act : io (Either err a))
  -> EitherT PackErr io a
eitherIO toErr = MkEitherT . map (mapFst toErr)

||| Make sure a *cleanup* action is run after
||| an IO action that might fail.
export
finally :
     {auto _ : Monad m}
  -> (cleanup : EitherT err m ())
  -> (act     : EitherT err m a)
  -> EitherT err m a
finally cleanup act = MkEitherT $ do
  res <- runEitherT act
  ignore $ runEitherT cleanup
  pure res

||| Runs a *pack* program, printing errors to standard out.
export
run : EitherT PackErr IO () -> IO ()
run (MkEitherT io) = io >>= either fatal pure

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
sys : HasIO io => (cmd : CmdArgList) -> EitherT PackErr io ()
sys cmd = do
  0 <- system $ escapeCmd cmd | n => throwE (Sys cmd n)
  pure ()

logCmdOutput :
     {auto _   : HasIO io}
  -> {auto ref : LogRef}
  -> (lvl : LogLevel)
  -> (msg : String)
  -> io ()
logCmdOutput lvl msg =
  when (msg /= "") $ log ref lvl msg

lineBufferedCmd : Env => CmdArgList -> CmdArgList
lineBufferedCmd args = lineBufferingCmd %search ++ args

||| Tries to run a system command while logging its output.
export covering
sysAndLog :
     {auto _ : HasIO io}
  -> {auto _ : Env}
  -> (lvl : LogLevel)
  -> (cmd : CmdArgList)
  -> EitherT PackErr io ()
sysAndLog lvl cmd = do
  0 <- runProcessingOutput
         (logCmdOutput lvl)
         (escapeCmd $ lineBufferedCmd cmd)
    | n => throwE (Sys cmd n)
  pure ()

cmdWithEnv : CmdArgList -> List (String,String) -> String
cmdWithEnv cmd []  = escapeCmd cmd
cmdWithEnv cmd env = "\{dispEnv env} \{escapeCmd cmd}"

||| Tries to run a system command prefixed with the given
||| environment variables.
|||
||| Note: In case of an error, the environment will not be part
||| of the command listed in the error message. This is a
||| deliberate choice to declutter Idris output in case of a
||| failed build. If the environment should be included in the
||| error message, just prefix `cmd` accordingly and use `sys`.
export
sysWithEnv :
     {auto _ : HasIO io}
  -> (cmd : CmdArgList)
  -> (env : List (String,String))
  -> EitherT PackErr io ()
sysWithEnv cmd env = do
  0 <- system (cmdWithEnv cmd env) | n => throwE (Sys cmd n)
  pure ()

export covering
sysWithEnvAndLog :
     {auto _ : HasIO io}
  -> {auto _ : Env}
  -> (lvl : LogLevel)
  -> (cmd : CmdArgList)
  -> (env : List (String,String))
  -> EitherT PackErr io ()
sysWithEnvAndLog lvl cmd env = do
  0 <- runProcessingOutput
         (logCmdOutput lvl)
         (cmdWithEnv (lineBufferedCmd cmd) env)
    | n => throwE (Sys cmd n)
  pure ()

||| Tries to run a system command returning its output.
export covering
sysRun : HasIO io => (cmd : CmdArgList) -> EitherT PackErr io String
sysRun cmd = do
  (res,0) <- run (escapeCmd cmd) | (_,n) => throwE (Sys cmd n)
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
sysRunWithEnv :
     {auto _ : HasIO io}
  -> (cmd : CmdArgList)
  -> (env : List (String,String))
  -> EitherT PackErr io String
sysRunWithEnv cmd env = do
  (res,0) <- System.run (cmdWithEnv cmd env) | (_,n) => throwE (Sys cmd n)
  pure res

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
mkDir d          = sys ["mkdir", "-p", d]

||| Creates a parent directory of a (file) path
export
mkParentDir : HasIO io => (p : Path Abs) -> EitherT PackErr io ()
mkParentDir p = whenJust (parentDir p) mkDir

||| Forcefully deletes a directory with all its content
export
rmDir : HasIO io => (dir : Path Abs) -> EitherT PackErr io ()
rmDir dir = when !(exists dir) $ sys ["rm", "-rf", dir]

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
inDir :
     {auto _ : HasIO io}
  -> (dir : Path Abs)
  -> (act : Path Abs -> EitherT PackErr io a)
  -> EitherT PackErr io a
inDir dir act =
  curDir >>= \cur => finally (chgDir cur) (chgDir dir >> act dir)

||| Returns the names of entries in a directory
export
entries :
     {auto _ : HasIO io}
  -> (dir : Path Abs)
  -> EitherT PackErr io (List Body)
entries dir = do
  ss <- eitherIO (DirEntries dir) (listDir "\{dir}")
  pure (mapMaybe parse ss)

||| Returns the names of toml files in a directory
export
tomlFiles :  {auto _ : HasIO io}
  -> (dir : Path Abs)
  -> EitherT PackErr io (List Body)
tomlFiles dir = filter isTomlBody <$> entries dir

||| Returns the names of toml files in a directory
export
htmlFiles :  {auto _ : HasIO io}
  -> (dir : Path Abs)
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
  sys ["cp", "-r", from, to]

||| Copy a whole directory into the given parent directory.
export
copyDirInto : HasIO io => (from,parent : Path Abs) -> EitherT PackErr io ()
copyDirInto from parent = do
  mkDir parent
  sys ["cp", "-r", from, "\{parent}/"]

||| Tries to find the first file, the body of which returns `True` for
||| the given predicate.
export
findInParentDirs :  {auto _ : HasIO io}
  -> (Body -> Bool)
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

||| Tries to find the first file, the body of which return `True` for
||| the given predicate, in each parent directory.
export
findInAllParentDirs :  {auto _ : HasIO io}
  -> (Body -> Bool)
  -> Path Abs
  -> EitherT PackErr io $ List $ File Abs
findInAllParentDirs p = go [] where
  go : List (File Abs) -> Path Abs -> EitherT PackErr io $ List $ File Abs
  go presentRes currD = do
    Just af <- findInParentDirs p currD
      | Nothing => pure presentRes
    let nextRes = af::presentRes
    case parentDir $ parent af of
      Just parentD => go nextRes $ assert_smaller currD parentD
      Nothing      => pure nextRes

mkTmpDir : HasIO io => PackDir => EitherT PackErr io TmpDir
mkTmpDir = go 100 0

  where
    go : Nat -> Nat -> EitherT PackErr io TmpDir
    go 0     _ = throwE NoTmpDir
    go (S k) n =
      let Just body := Body.parse ".tmp\{show n}" | Nothing => go k (S n)
          dir       := packDir /> body
       in do
         False <- exists dir | True => go k (S n)
         when (n > 50) $
           warn {ref = MkLogRef Info}
             """
             Too many temporary directories. Please remove all `.tmpXY`
             directories in `PACK_DIR` or run `pack gc` to let pack
             clean them up.
             """
         mkDir dir
         pure (TD dir)

export
withTmpDir :
     {auto _ : HasIO io}
  -> {auto _ : PackDir}
  -> (TmpDir => EitherT PackErr io a)
  -> EitherT PackErr io a
withTmpDir f = do
  td <- mkTmpDir
  finally (rmDir tmpDir) f

  --------------------------------------------------------------------------------
  --         File Access
  --------------------------------------------------------------------------------

||| Delete a file.
export
rmFile : HasIO io => (f : File Abs) -> EitherT PackErr io ()
rmFile f = when !(fileExists f) $ sys ["rm", f]

||| Tries to read the content of a file
export covering
read : HasIO io => File Abs -> EitherT PackErr io String
read fn = eitherIO (ReadFile fn) (readFile "\{fn}")

||| Reads the content of a file if it exists, otherwise
||| returns the given alternative string.
export covering
readIfExists :  {auto _ : HasIO io}
  -> (file : File Abs)
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
  sys ["ln", "-s", from, to]

||| Copy a file.
export
copyFile : HasIO io => (from,to : File Abs) -> EitherT PackErr io ()
copyFile from to = do
  mkDir to.parent
  sys ["cp", from, to]

||| Patch a file
export
patch :  {auto _ : HasIO io}
  -> (original : File Abs)
  -> (patch    : File Abs)
  -> EitherT PackErr io ()
patch o p = sys ["patch", o, p]
