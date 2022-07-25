module Pack.Config.Environment

import Data.Maybe
import Data.SortedMap as SM
import Idris.Package.Types
import Pack.CmdLn
import Pack.Config.TOML
import Pack.Config.Types
import Pack.Core
import Pack.Database
import System

%default total

--------------------------------------------------------------------------------
--          Files and Directories
--------------------------------------------------------------------------------

export
packToml : Body
packToml = "pack.toml"

||| Temporary directory used for building packages.
export %inline
tmpDir : PackDir => Path Abs
tmpDir = packDir /> ".tmp"

||| Clone of the pack GitHub repo
export %inline
packClone : PackDir => Path Abs
packClone = tmpDir /> "pack"

||| Directory where databases are stored.
export %inline
dbDir : PackDir => Path Abs
dbDir = packDir /> "db"

||| Directory where databases are stored.
export %inline
cacheDir : PackDir => Path Abs
cacheDir = packDir /> ".cache"

||| Path to cached `.ipkg` file.
export
ipkgCachePath : PackDir => PkgName -> Commit -> File Rel -> File Abs
ipkgCachePath p com = toAbsFile (cacheDir <//> p <//> com)

||| Path to cached core library `.ipkg` file
export
coreCachePath : PackDir => (db : DB) => CorePkg -> File Abs
coreCachePath n =
  MkF (cacheDir <//> n <//> db.idrisCommit) (coreIpkgFile n)

||| Directory where user settings are stored.
export %inline
userDir : PackDir => Path Abs
userDir = packDir /> "user"

||| Path to global `pack.toml` file.
export %inline
globalPackToml : PackDir => File Abs
globalPackToml = MkF userDir packToml

||| File where package DB is located
export
dbFile : PackDir => (c : Config) => File Abs
dbFile = MkF dbDir $ c.collection.value <+> ".toml"

||| Directory where wrapper scripts to binaries
||| managed by pack are being stored. The only exception
||| is pack itself, which is stored as a symbolic link.
export %inline
packBinDir : PackDir => Path Abs
packBinDir = packDir /> "bin"

export %inline
packInstallDir : PackDir => Commit -> Path Abs
packInstallDir com = packDir </> "install/pack" </> cast com

||| Executable for an application
export %inline
pathExec : PackDir => Body -> File Abs
pathExec b = MkF packBinDir b

||| Symbolic link to the current pack executable.
export
packExec : PackDir => File Abs
packExec = pathExec "pack"

||| Directory where `.ipkg` patches are stored.
export
patchesDir : PackDir => Path Abs
patchesDir = dbDir /> "patches"

||| File where the patch (if any) for an `.ipkg` file is stored.
export
patchFile : PackDir => (c : Config) => PkgName -> File Rel -> File Abs
patchFile n (MkF p b) = MkF
  (patchesDir //> c.collection <//> n </> p)
  (b <+> ".patch")

||| Directory where all packages (and Idris2) built with the
||| current Idris2 commit will be installed.
export %inline
commitDir : PackDir => (db : DB) => Path Abs
commitDir = packDir </> "install" <//> db.idrisCommit

||| The directory where Idris2 and core libraries will be installed.
export %inline
idrisPrefixDir : PackDir => DB => Path Abs
idrisPrefixDir = commitDir /> "idris2"

||| The directory where the Idris2 binary will be installed.
export %inline
idrisBinDir : PackDir => DB => Path Abs
idrisBinDir = idrisPrefixDir /> "bin"

||| Location of the Idris2 executable used to build
||| packages.
export
idrisExec : PackDir => DB => File Abs
idrisExec = MkF idrisBinDir "idris2"

%inline
idrisDir : PackDir => (db : DB) => Body
idrisDir = the Body "idris2" <-> db.idrisVersion

||| The directory where Idris2 packages will be installed.
export %inline
idrisInstallDir : PackDir => DB => Path Abs
idrisInstallDir = idrisPrefixDir /> idrisDir

||| The `lib` directory in the Idris2 installation folder
export %inline
idrisLibDir : PackDir => DB => Path Abs
idrisLibDir = idrisInstallDir /> "lib"

||| The `support` directory in the Idris2 installation folder
export %inline
idrisDataDir : PackDir => DB => Path Abs
idrisDataDir = idrisInstallDir /> "support"

export %inline
pkgPrefixDir : PackDir => DB => PkgName -> Package -> Path Abs
pkgPrefixDir n (GitHub _ c _ _) = commitDir <//> n <//> c
pkgPrefixDir n (Local _ _ _)    = commitDir </> "local" <//> n
pkgPrefixDir n (Core _)         = idrisPrefixDir

export %inline
pkgPathDir : PackDir => DB => PkgName -> Package -> Path Abs
pkgPathDir n p = pkgPrefixDir n p /> idrisDir

export %inline
pkgBinDir : PackDir => DB => PkgName -> Package -> Path Abs
pkgBinDir n p = pkgPrefixDir n p /> "bin"

export %inline
pkgLibDir : PackDir => DB => PkgName -> Package -> Path Abs
pkgLibDir n p = pkgPathDir n p /> "lib"

export %inline
pkgDataDir : PackDir => DB => PkgName -> Package -> Path Abs
pkgDataDir n p = pkgPathDir n p /> "support"

||| Directory where the API docs of the package will be installed.
export %inline
pkgDocs : PackDir => DB => PkgName -> Package -> Path Abs
pkgDocs n p = pkgPrefixDir n p /> "docs"

||| Timestamp used to monitor if a local library has been
||| modified and requires reinstalling.
export %inline
libTimestamp :  PackDir => DB => PkgName -> Package -> File Abs
libTimestamp n p = MkF (pkgPathDir n p) ".timestamp"

||| Timestamp used to monitor if a local app has been
||| modified and requires reinstalling.
export %inline
appTimestamp :  PackDir => DB => PkgName -> Package -> File Abs
appTimestamp n p = MkF (pkgBinDir n p) ".timestamp"

||| Directory where the sources of a local package are
||| stored.
export %inline
localSrcDir : Desc t -> Path Abs
localSrcDir d = sourcePath d

pkgRelDir : Desc t -> Path Rel
pkgRelDir d = case Body.parse d.desc.name of
  Just b  => neutral /> (b <-> d.desc.version)
  Nothing => cast d.desc.name //> d.desc.version

||| Returns the directory where a package for the current
||| package collection is installed.
export
pkgInstallDir : PackDir => (db : DB) => PkgName -> Package -> Desc t -> Path Abs
pkgInstallDir n p d =
  let vers = db.idrisVersion
      dir  = pkgPrefixDir n p /> idrisDir
   in case p of
        Core c         => dir /> (c <-> vers)
        GitHub _ _ _ _ => dir </> pkgRelDir d
        Local _ _ _    => dir </> pkgRelDir d

||| Location of an executable of the given name.
export
pkgExec : PackDir => DB => PkgName -> Package -> (exe : Body) -> File Abs
pkgExec n p exe = MkF (pkgBinDir n p) exe

export
resolvedExec : PackDir => DB => ResolvedApp t -> File Abs
resolvedExec (RA p n d _ exe) = pkgExec n p exe

pathDirs :  (HasIO io, PackDir, DB, Config)
         => (pre : String)
         -> (pth : PkgName -> Package -> Path Abs)
         -> io String
pathDirs pre pth = do
  ps <- filterM (\(n,p) => exists $ pth n p) (SM.toList allPackages)
  let ps' := filter (not . isCorePkg . value . fst) ps
  pure $ fastConcat
       . intersperse ":"
       . (pre ::)
       $ map (\(n,p) => "\{pth n p}") ps'

export
packagePathDirs : HasIO io => PackDir => Config => DB -> io String
packagePathDirs _ = pathDirs "\{idrisInstallDir}" pkgPathDir

export
packageLibDirs : HasIO io => PackDir => Config => DB -> io String
packageLibDirs _ = pathDirs "\{idrisLibDir}" pkgLibDir

export
packageDataDirs : HasIO io => PackDir => Config => DB -> io String
packageDataDirs _ = pathDirs "\{idrisDataDir}" pkgDataDir

--------------------------------------------------------------------------------
--          Environment Variables
--------------------------------------------------------------------------------

||| `$PREFIX` variable during Idris2 installation
export
prefixVar : PackDir => DB => String
prefixVar = "PREFIX=\{quote idrisPrefixDir}"

||| `$IDRIS2_BOOT` variable during Idris2 installation
export
idrisBootVar : PackDir => DB => String
idrisBootVar = "IDRIS2_BOOT=\{quote idrisExec}"

||| `$SCHEME` variable during Idris2 installation
export
schemeVar : (c : Config) => String
schemeVar = "SCHEME=\{quote c.scheme}"

export
libInstallPrefix : PackDir => DB => ResolvedLib t -> List (String,String)
libInstallPrefix rl =
  [("IDRIS2_PREFIX", "\{pkgPrefixDir rl.name rl.pkg}")]

export
packagePath : HasIO io => Env => io (String, String)
packagePath =
  ("IDRIS2_PACKAGE_PATH",) <$>  packagePathDirs %search

export
libPath : HasIO io => Env => io (String, String)
libPath = ("IDRIS2_LIBS",) <$> packageLibDirs %search

export
dataPath : HasIO io => Env => io (String, String)
dataPath = ("IDRIS2_DATA",) <$> packageDataDirs %search

export
buildEnv : HasIO io => Env => io (List (String,String))
buildEnv = sequence [packagePath, libPath, dataPath]

||| Idris executable to use together with the
||| `--cg` (codegen) command line option.
export
idrisWithCG : (e : Env) => String
idrisWithCG = case e.config.codegen of
  Default => "\{idrisExec}"
  cg      => "\{idrisExec} --cg \{cg}"

||| Idris executable loading the given package plus the
||| environment variables needed to run it.
export
idrisWithPkg :  HasIO io
             => IdrisEnv
             => ResolvedLib t
             -> io (String, List (String,String))
idrisWithPkg rl =
  ("\{idrisWithCG} -p \{name rl}",) <$> buildEnv

||| Idris executable loading the given packages plus the
||| environment variables needed to run it.
export
idrisWithPkgs :  HasIO io
              => IdrisEnv
              => List (ResolvedLib t)
              -> io (String, List (String,String))
idrisWithPkgs [] = pure (idrisWithCG, [])
idrisWithPkgs pkgs =
  let ps = fastConcat $ map (\p => " -p \{name p}") pkgs
   in ("\{idrisWithCG}\{ps}",) <$> buildEnv

--------------------------------------------------------------------------------
--          Logging
--------------------------------------------------------------------------------

export
log :  HasIO io
    => (ref : LogLevel)
    -> (lvl : LogLevel)
    -> (msg : Lazy String)
    -> io ()
log ref lvl msg =
  when (lvl >= ref) (putStrLn "[ \{lvl} ] \{msg}")

export
logMany :  HasIO io
        => (ref  : LogLevel)
        => (lvl  : LogLevel)
        -> (msg  : Lazy String)
        -> (msgs : Lazy (List String))
        -> io ()
logMany lvl msg msgs =
  when (lvl >= ref && not (null msgs)) $ do
    log ref lvl $ unlines (msg :: map (indent 2) msgs)

export %inline
debug : HasIO io => (ref : LogLevel) => (msg  : Lazy String) -> io ()
debug = log ref Debug

export %inline
info : HasIO io => (ref : LogLevel) => (msg  : Lazy String) -> io ()
info = log ref Info

export %inline
warn : HasIO io => (ref : LogLevel) => (msg  : Lazy String) -> io ()
warn = log ref Warning

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

getEnvPath : HasIO io => String -> io (Maybe (Path Abs))
getEnvPath s = (>>= parse) <$> getEnv s

||| Return the path of the pack root directory,
||| either from environment variable `$PACK_DIR`, or
||| as `$HOME/.pack`.
export
getPackDir : HasIO io => EitherT PackErr io PackDir
getPackDir = do
  Nothing <- getEnvPath "PACK_DIR" | Just v => pure $ PD v
  Nothing <- getEnvPath "HOME"     | Just v => pure $ PD (v /> ".pack")
  throwE NoPackDir

||| Update the package database.
export
updateDB : HasIO io => PackDir => EitherT PackErr io ()
updateDB = do
  rmDir dbDir
  finally (rmDir tmpDir) $
    withGit tmpDir packDB dbRepo "main" $ \d =>
      copyDir (d /> "collections") dbDir

||| Loads the name of the default collection (currently the latest
||| nightly)
export
defaultColl : HasIO io => PackDir => EitherT PackErr io DBName
defaultColl = do
  when !(missing dbDir) updateDB
  (x :: xs) <- filter ("HEAD.toml" /=) <$> tomlFiles dbDir
    | [] => pure Head
  pure
    . maybe Head MkDBName
    . fileStem
    $ foldl max x xs

export
resolveMeta : HasIO io => UserPackage -> EitherT PackErr io Package
resolveMeta (GitHub u (MC x) i p) = pure $ GitHub u x i p
resolveMeta (GitHub u (Latest x) i p) =
  map (\c => GitHub u c i p) $ gitLatest u (MkCommit x)
resolveMeta (Local d i p) = pure $ Local d i p
resolveMeta (Core c)      = pure $ Core c

||| Read application config from command line arguments.
export covering
getConfig :  (0 c : Type)
          -> Command c
          => HasIO io
          => (pd        : PackDir)
          => (cur       : CurDir)
          => EitherT PackErr io (MetaConfig,c)
getConfig c = do
  -- relevant directories
  coll       <- defaultColl

  -- Initialize `pack.toml` if none exists
  when !(fileMissing globalPackToml) $
    write globalPackToml (initToml "scheme" coll)

  localToml   <- findInParentDirs ("pack.toml" ==) curDir
  global      <- readOptionalFromTOML globalPackToml config
  local       <- case localToml of
    Just af => readFromTOML af config
    Nothing => readOptionalFromTOML (MkF curDir packToml) config

  let ini = init coll `update` global `update` local

  pn :: args  <- getArgs | Nil => pure (ini, defaultCommand c)
  (conf',cmd) <- liftEither $ applyArgs c cur ini args
  conf        <- adjConfig cmd conf'

  let level := conf.logLevel

  debug "Pack home is \{pd}"
  debug "Current directory is \{cur}"
  case localToml of
    Just af => info "Found local config at \{af}"
    Nothing => debug "No local config found"
  info "Using package collection \{conf.collection}"
  debug "Config loaded"
  mkDir packDir
  pure (conf,cmd)

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

pkgs : SortedMap PkgName Package
pkgs = fromList $ (\c => (corePkgName c, Core c)) <$> corePkgs

export covering
loadDB : HasIO io => PackDir => Config => EitherT PackErr io DB
loadDB = do
  when !(missing dbDir) updateDB
  debug "reading package collection"
  readFromTOML dbFile db

export covering
env : HasIO io => (pd : PackDir) => (c : Config) => EitherT PackErr io Env
env = MkEnv pd c <$> loadDB

adjCollection : DBName -> String -> String
adjCollection db str = case isPrefixOf "collection " str of
  False => str
  True  => "collection = \{quote db}"

export covering
writeCollection :  HasIO io
                => PackDir
                => (c : Config)
                => EitherT PackErr io ()
writeCollection = do
  str <- read globalPackToml
  write globalPackToml (unlines . map (adjCollection c.collection) $ lines str)
