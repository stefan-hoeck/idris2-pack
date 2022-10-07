module Pack.Config.Environment

import Data.Maybe
import Data.SortedMap as SM
import Data.String
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

||| File body of all `pack.toml` files.
export
packToml : Body
packToml = "pack.toml"

||| Clone of the pack GitHub repo
export %inline
packClone : TmpDir => Path Abs
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
dbFile : PackDir => (c : MetaConfig) => File Abs
dbFile = MkF dbDir $ c.collection.value <+> ".toml"

||| Directory where wrapper scripts to binaries
||| managed by pack are being stored. The only exception
||| is pack itself, which is stored as a symbolic link.
export %inline
packBinDir : PackDir => Path Abs
packBinDir = packDir /> "bin"

||| Where the actual pack application is installed.
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

||| Path to file storing the last fetched commit of a GitHub
||| repo given as a URL and branch name.
export
commitFile : PackDir => URL -> Branch -> File Abs
commitFile url b =
  let relPath = the (Path Rel) $ cast "\{url}/\{b}"
   in MkF (cacheDir </> relPath) "commit"

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

||| Directory where an installed library or app goes
export %inline
pkgPrefixDir : PackDir => DB => PkgName -> Package -> Path Abs
pkgPrefixDir n (GitHub _ c _ _) = commitDir <//> n <//> c
pkgPrefixDir n (Local _ _ _)    = commitDir </> "local" <//> n
pkgPrefixDir n (Core _)         = idrisPrefixDir

||| Directory to be used with the `IDRIS2_PACKAGE_PATH` variable, so that
||| Idris finds a library even though it is being installed in a
||| custom location.
export %inline
pkgPathDir : PackDir => DB => PkgName -> Package -> Path Abs
pkgPathDir n p = pkgPrefixDir n p /> idrisDir

||| Directory where the binary of an Idris application is installed.
export %inline
pkgBinDir : PackDir => DB => PkgName -> Package -> Path Abs
pkgBinDir n p = pkgPrefixDir n p /> "bin"

||| Directory to be used with the `IDRIS2_LIBS` variable, so that
||| Idris finds a libraries `.so` files even though they have been
||| installed in a custom location.
export %inline
pkgLibDir : PackDir => DB => PkgName -> Package -> Path Abs
pkgLibDir n p = pkgPathDir n p /> "lib"

||| Directory to be used with the `IDRIS2_DATA` variable, so that
||| Idris finds a libraries support files even though they have been
||| installed in a custom location.
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

||| Path to the executable of an Idris application
export
resolvedExec : PackDir => DB => ResolvedApp t -> File Abs
resolvedExec (RA p n d _ exe _) = pkgExec n p exe

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

||| Directories to be listed in the `IDRIS2_PACKAGE_PATH` variable, so
||| that Idris finds all libraries installed by pack in custom locations.
export
packagePathDirs : HasIO io => Env -> io String
packagePathDirs _ = pathDirs "\{idrisInstallDir}" pkgPathDir

||| Directories to be listed in the `IDRIS2_LIBS` variable, so
||| that Idris finds all `.so` files installed by pack in custom locations.
export
packageLibDirs : HasIO io => Env -> io String
packageLibDirs _ = pathDirs "\{idrisLibDir}" pkgLibDir

||| Directories to be listed in the `IDRIS2_DATA` variable, so
||| that Idris finds all support files installed by pack in custom locations.
export
packageDataDirs : HasIO io => Env -> io String
packageDataDirs _ = pathDirs "\{idrisDataDir}" pkgDataDir

||| URL of the pack repository to use
export
packRepo : (c : Config) => URL
packRepo = fromMaybe defaultPackRepo c.packURL

||| URL of the pack repository to use
export
packCommit : (c : Config) => Maybe Commit
packCommit = c.packCommit

--------------------------------------------------------------------------------
--          Environment Variables
--------------------------------------------------------------------------------

||| `$PREFIX` variable during Idris2 installation, unquoted
export
prefixVar : PackDir => DB => String
prefixVar = "PREFIX=\{idrisPrefixDir}"

||| `$IDRIS2_BOOT` variable during Idris2 installation, unquoted
export
idrisBootVar : PackDir => DB => String
idrisBootVar = "IDRIS2_BOOT=\{idrisExec}"

||| `$SCHEME` variable during Idris2 installation, unquoted
export
schemeVar : (c : Config) => String
schemeVar = "SCHEME=\{c.scheme}"

||| `IDRIS2_PREFIX` to be used with Idris when installing a library
||| to a custom location.
export
libInstallPrefix : PackDir => DB => ResolvedLib t -> List (String,String)
libInstallPrefix rl =
  [("IDRIS2_PREFIX", "\{pkgPrefixDir rl.name rl.pkg}")]

||| `IDRIS2_PACKAGE_PATH` variable to be used with Idris, so
||| that it finds all libraries installed by pack in custom locations.
export
packagePath : HasIO io => Env => io (String, String)
packagePath =
  ("IDRIS2_PACKAGE_PATH",) <$>  packagePathDirs %search

||| `IDRIS2_LIBS` variable to be used with Idris, so
||| that it finds all `.so` files installed by pack in custom locations.
export
libPath : HasIO io => Env => io (String, String)
libPath = ("IDRIS2_LIBS",) <$> packageLibDirs %search

||| `IDRIS2_DATA` variable to be used with Idris, so
||| that it finds all support files installed by pack in custom locations.
export
dataPath : HasIO io => Env => io (String, String)
dataPath = ("IDRIS2_DATA",) <$> packageDataDirs %search

||| This unifies `packagePath`, `libPath` and `dataPath`,
||| to generate an environment necessary to build packages with Idris
||| the dependencies of which are handled by pack.
export
buildEnv : HasIO io => Env => io (List (String,String))
buildEnv = sequence [packagePath, libPath, dataPath]

||| Idris executable to use together with the
||| `--cg` (codegen) command line option.
export
idrisWithCG : (e : Env) => CmdArgList
idrisWithCG = case e.config.codegen of
  Default => [idrisExec]
  cg      => [idrisExec, "--cg", cg]

||| Idris executable loading the given package plus the
||| environment variables needed to run it.
export
idrisWithPkg :  HasIO io
             => IdrisEnv
             => ResolvedLib t
             -> io (CmdArgList, List (String,String))
idrisWithPkg rl =
  (idrisWithCG ++ ["-p", name rl],) <$> buildEnv

||| Idris executable loading the given packages plus the
||| environment variables needed to run it.
export
idrisWithPkgs :  HasIO io
              => IdrisEnv
              => List (ResolvedLib t)
              -> io (CmdArgList, List (String,String))
idrisWithPkgs [] = pure (idrisWithCG, [])
idrisWithPkgs pkgs =
  let ps = concatMap (\p => ["-p", name p]) pkgs
   in (idrisWithCG ++ ps,) <$> buildEnv

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
updateDB : HasIO io => TmpDir => PackDir => EitherT PackErr io ()
updateDB = do
  rmDir dbDir
  finally (rmDir tmpDir) $
    withGit tmpDir packDB dbRepo "main" $ \d =>
      copyDir (d /> "collections") dbDir

||| Loads the name of the default collection (currently the latest
||| nightly)
export
defaultColl : HasIO io => TmpDir => PackDir => EitherT PackErr io DBName
defaultColl = do
  when !(missing dbDir) updateDB
  (x :: xs) <- filter ("HEAD.toml" /=) <$> tomlFiles dbDir
    | [] => pure Head
  pure
    . maybe Head MkDBName
    . fileStem
    $ foldl max x xs

||| Resolve a meta commit by fetching the hash of the latest commit
||| from GitHub in case of an `Fetch x` commit. In case of a `Latest x`
||| meta commit, the hash is only fetched, if the corresponding commit
||| file is missing or `fetch` is set to `True`.
export
resolveMeta :  HasIO io
            => PackDir
            => (fetch : Bool)
            -> URL
            -> MetaCommit
            -> EitherT PackErr io Commit
resolveMeta _ u (MC x)     = pure x
resolveMeta _ u (Fetch x)  = gitLatest u x
resolveMeta b u (Latest x) = do
  let cfile := commitFile u x
  commitMissing <- fileMissing cfile
  case commitMissing || b of
    True  => do
      c <- gitLatest u x
      write cfile c.value
      pure c
    False => (\s => MkCommit $ trim s) <$> read cfile

covering
findLocalTOMLs :  HasIO io
               => List (File Abs, UserConfig)
               -> Path Abs
               -> EitherT PackErr io $ List (File Abs, UserConfig)
findLocalTOMLs presentConf currD = do
  Just af <- findInParentDirs (packToml ==) currD
    | Nothing => case presentConf of
                   -- No local `pack.toml` found and it is okay
                   []   => pure []
                   -- No local toplevel `pack.toml` is found
                   _::_ => throwError $ TopLevelConfigExpected currD $ fst <$> presentConf
  local <- readFromTOML UserConfig af
  let nextConf = (af, local) :: presentConf
  if local.toplevel /= Just False
    then pure nextConf
    else do
      let Just parentD = parentDir $ parent af
          -- No local toplevel `pack.toml` is found and nowhere to look for it
        | Nothing => throwError $ TopLevelConfigExpected currD $ fst <$> nextConf
      findLocalTOMLs nextConf parentD

||| Read application config from command line arguments.
export covering
getConfig :  (0 c : Type)
          -> Command c
          => HasIO io
          => (pd        : PackDir)
          => (td        : TmpDir)
          => (cur       : CurDir)
          => EitherT PackErr io (MetaConfig,c)
getConfig c = do
  -- relevant directories
  coll       <- defaultColl

  -- Initialize `pack.toml` if none exists
  when !(fileMissing globalPackToml) $
    write globalPackToml (initToml "scheme" coll)

  (localTOMLs, localConfs) <- unzip <$> findLocalTOMLs [] curDir
  global      <- readOptionalFromTOML UserConfig globalPackToml

  let ini = foldl update (init coll `update` global) localConfs

  pn :: args  <- getArgs | Nil => pure (ini, defaultCommand c)
  (conf',cmd) <- liftEither $ applyArgs c cur ini args
  conf        <- adjConfig cmd conf'

  let level := conf.logLevel

  debug "Pack home is \{pd}"
  debug "Current directory is \{cur}"
  case localTOMLs of
    _::_ => info "Found local config at \{joinBy ", " $ interpolate <$> localTOMLs}"
    []   => debug "No local config found"
  info "Using package collection \{conf.collection}"
  debug "Config loaded"
  mkDir packDir
  pure (conf,cmd)

export
getLineBufferingCmd : HasIO io => io LineBufferingCmd
getLineBufferingCmd = findCmd variants
  where
    findCmd : List (String, CmdArgList) -> io LineBufferingCmd
    findCmd [] = pure $ MkLineBufferingCmd []
    findCmd ((cmd, args)::rest) = do
      0 <- system $ escapeCmd
             ["type", cmd, NoEscape ">", "/dev/null", NoEscape "2>", "/dev/null"]
        | _ => findCmd rest
      pure $ MkLineBufferingCmd $ [cmd] ++ args

    variants : List (String, CmdArgList)
    variants = [ ("stdbuf",  ["-oL"])
               , ("gstdbuf", ["-oL"])
               ]

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

pkgs : SortedMap PkgName Package
pkgs = fromList $ (\c => (corePkgName c, Core c)) <$> corePkgs

||| Load the package collection as given in the (auto-implicit) user config.
export covering
loadDB : HasIO io => TmpDir => PackDir => MetaConfig -> EitherT PackErr io DB
loadDB mc = do
  when !(missing dbDir) updateDB
  debug "reading package collection"
  readFromTOML DB dbFile

||| Load the package collection as given in the (auto-implicit) user config
||| and convert the result to a pack environment.
export covering
env :  HasIO io
    => (pd    : PackDir)
    => (td    : TmpDir)
    => (ch    : LibCache)
    => (lbf   : LineBufferingCmd)
    => (c     : MetaConfig)
    -> (fetch : Bool)
    -> EitherT PackErr io Env
env mc fetch = do
  db <- loadDB mc
  c  <- traverse (resolveMeta fetch) db.idrisURL mc

  let url    := fromMaybe db.idrisURL c.idrisURL
      commit := fromMaybe db.idrisCommit c.idrisCommit

  -- adjust the idrisCommit and URL to use according to user overrides
  pure $ MkEnv pd td c ch ({idrisURL := url, idrisCommit := commit} db) lbf

adjCollection : DBName -> String -> String
adjCollection db str = case isPrefixOf "collection " str of
  False => str
  True  => "collection = \{quote db}"

||| Update the `collection` field in file `PACK_DIR/user/pack.toml`
||| with the name of the package collection given in config `c`.
export covering
writeCollection :  HasIO io
                => PackDir
                => (c : Config)
                => EitherT PackErr io ()
writeCollection = do
  str <- read globalPackToml
  write globalPackToml (unlines . map (adjCollection c.collection) $ lines str)
