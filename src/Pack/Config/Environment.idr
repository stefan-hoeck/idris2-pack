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

||| Where packages and built applications will be installed
export %inline
installDir : PackDir => Path Abs
installDir = packDir </> "install"

||| Where all pack versions are installed
export %inline
packParentDir : PackDir => Path Abs
packParentDir = installDir </> "pack"

||| Where the actual pack application is installed.
export %inline
packInstallDir : PackDir => Commit -> Path Abs
packInstallDir com = packParentDir </> cast com

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

||| Path to file storing the last fetched commit of a Git
||| repo given as a URL and branch name.
export
commitFile : PackDir => URL -> Branch -> File Abs
commitFile url b =
  let relPath := the (Path Rel) $ cast "\{url}/\{b}"
   in MkF (cacheDir </> relPath) "commit"

||| File where the patch (if any) for an `.ipkg` file is stored.
export
patchFile : PackDir => (c : Config) => PkgName -> File Rel -> File Abs
patchFile n (MkF p b) =
  MkF
    (patchesDir //> c.collection <//> n </> p)
    (b <+> ".patch")

||| Directory where all packages (and Idris2) built with the
||| current Idris2 commit will be installed.
export %inline
commitDir : PackDir => (db : DB) => Path Abs
commitDir = installDir <//> db.idrisCommit

||| The directory where Idris2 and core libraries will be installed.
export %inline
idrisPrefixDir : PackDir => DB => Path Abs
idrisPrefixDir = commitDir /> "idris2"

||| The directory where the Idris2 binary will be installed.
export %inline
idrisBinDir : PackDir => DB => Path Abs
idrisBinDir = idrisPrefixDir /> "bin"

||| Location of the Idris2 executable used to build packages.
|||
||| Notice that if you need an Idris command, you may need `idrisCmd` function
||| instead because it takes extra arguments into account.
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
pkgPrefixDir n (Git _ c _ _ _) = commitDir <//> n <//> c
pkgPrefixDir n (Local _ _ _ _) = commitDir </> "local" <//> n
pkgPrefixDir n (Core _)        = idrisPrefixDir

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
  let vers := db.idrisVersion
      dir  := pkgPrefixDir n p /> idrisDir
   in case p of
        Core c        => dir /> (c <-> vers)
        Git _ _ _ _ _ => dir </> pkgRelDir d
        Local _ _ _ _ => dir </> pkgRelDir d

||| Directory where the API docs of the package will be installed.
export %inline
pkgDocs : PackDir => DB => PkgName -> Package -> Desc t -> Path Abs
pkgDocs n p d = pkgInstallDir n p d /> "docs"

||| Location of an executable of the given name.
export
pkgExec : PackDir => DB => PkgName -> Package -> (exe : Body) -> File Abs
pkgExec n p exe = MkF (pkgBinDir n p) exe

||| Path to the executable of an Idris application
export
resolvedExec : PackDir => DB => ResolvedApp t -> File Abs
resolvedExec (RA p n d _ exe _) = pkgExec n p exe

pathDirs :
     {auto _ : HasIO io}
  -> {auto _ : PackDir}
  -> {auto _ : DB}
  -> {auto _ : Config}
  -> (pre : String)
  -> (pth : PkgName -> Package -> Path Abs)
  -> io String
pathDirs pre pth = do
  ps <- filterM (\(n,p) => exists $ pth n p) (SM.toList allPackages)
  let ps' := filter (not . isCorePkg . value . fst) ps
  pure $
      fastConcat
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
export %inline
packRepo : (c : Config) => URL
packRepo = fromMaybe defaultPackRepo c.packURL

||| Commit of pack to use
export %inline
packCommit : (c : Config) => Maybe Commit
packCommit = c.packCommit

||| True if the path to the scheme executable actually points
||| to `racket`.
export
useRacket : (c : Config) => Bool
useRacket = map snd (split c.scheme) == Just "racket"

||| Bootstrap command to use
export
bootstrapCmd : (c : Config) => String
bootstrapCmd = if useRacket then "bootstrap-racket" else "bootstrap"

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
schemeVar = if useRacket then "IDRIS2_CG=racket" else "SCHEME=\{c.scheme}"

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
buildEnv =
  let pre := if useRacket then [("IDRIS2_CG", "racket")] else []
   in (pre ++ ) <$> sequence [packagePath, libPath, dataPath]

||| Idris executable with extra arguments, if they are present in the config.
export
idrisCmd : (e : Env) => CmdArgList
idrisCmd = idrisExec :: e.config.extraArgs

||| Idris executable to use together with the
||| `--cg` (codegen) command line option.
export
idrisWithCG : (e : Env) => CmdArgList
idrisWithCG = case e.config.codegen of
  Default => idrisCmd
  cg      => idrisCmd ++ ["--cg", cg]

||| Idris executable loading the given package plus the
||| environment variables needed to run it.
export
idrisWithPkg :
     {auto _ : HasIO io}
  -> {auto _ : IdrisEnv}
  -> ResolvedLib t
  -> io (CmdArgList, List (String,String))
idrisWithPkg rl =
  (idrisWithCG ++ ["-p", name rl],) <$> buildEnv

||| Idris executable loading the given packages plus the
||| environment variables needed to run it.
export
idrisWithPkgs :
     {auto _ : HasIO io}
  -> {auto _ : IdrisEnv}
  -> List (ResolvedLib t)
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
  commit <- gitLatest dbRepo "main"
  withGit packDB dbRepo commit $ \d =>
    copyDir (d /> "collections") dbDir

||| Extract the name of the latest collection from a directory
export
latestCollection : HasIO io => (dir : Path Abs) -> EitherT PackErr io DBName
latestCollection dir = do
  (x :: xs) <- filter ("HEAD.toml" /=) <$> tomlFiles dir
    | [] => pure Head
  pure
    . maybe Head MkDBName
    . fileStem
    $ foldl max x xs

||| Update the package database.
export
copyLatest : HasIO io => TmpDir => PackDir => EitherT PackErr io DBName
copyLatest = do
  commit <- gitLatest dbRepo "main"
  withGit packDB dbRepo commit $ \d => do
    db <- latestCollection (d /> "collections")
    let body := cast {to = Body} db <+> ".toml"
    copyFile (d /> "collections" /> body) (dbDir /> body)
    pure db

||| Loads the name of the default collection (currently the latest
||| nightly)
export
defaultColl : HasIO io => TmpDir => PackDir => EitherT PackErr io DBName
defaultColl = do
  when !(missing dbDir) updateDB
  latestCollection dbDir

||| Resolve a meta commit by fetching the hash of the latest commit
||| from a Git repo in case of an `Fetch x` commit. In case of a `Latest x`
||| meta commit, the hash is only fetched, if the corresponding commit
||| file is missing or `fetch` is set to `True`.
export
resolveMeta :
     {auto _ : HasIO io}
  -> {auto _ : PackDir}
  -> (fetch : Bool)
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

||| Read application config from command line arguments.
export covering
getConfig :
     (0 c : Type)
  -> {auto _   : Command c}
  -> {auto _   : HasIO io}
  -> {auto pd  : PackDir}
  -> {auto td  : TmpDir}
  -> {auto cur : CurDir}
  -> EitherT PackErr io (MetaConfig, CommandWithArgs c)
getConfig c = do
  -- relevant directories
  coll       <- defaultColl

  -- Initialize `pack.toml` if none exists
  when !(fileMissing globalPackToml) $
    write globalPackToml (initToml "scheme" coll)

  localTomls  <- findInAllParentDirs (packToml ==) curDir
  localConfs  <- for localTomls $ readFromTOML UserConfig
  global      <- readOptionalFromTOML UserConfig globalPackToml

  let ini = foldl update (init coll `update` global) localConfs

  args'       <- getArgs
  let args : List String
      args = case args' of
        h :: t => t
        []     => [] -- this should not happen

  (conf',cmd) <- liftEither $ applyArgs c cur ini args
  conf        <- adjConfig cmd conf'

  let logRef := MkLogRef conf.logLevel

  debug "Pack home is \{pd}"
  debug "Current directory is \{cur}"
  case localTomls of
    _::_ =>
      logMany
        Info
        {inlineSingle=True}
        "Found local config at"
        (interpolate <$> localTomls)
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
    variants =
      [ ("stdbuf",  ["-oL"])
      , ("gstdbuf", ["-oL"])
      ]

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

pkgs : SortedMap PkgName Package
pkgs = fromList $ (\c => (corePkgName c, Core c)) <$> corePkgs

||| Load the package collection as given in the (auto-implicit) user config.
export covering
loadDB :
     {auto _ : HasIO io}
  -> {auto _ : TmpDir}
  -> {auto _ : PackDir}
  -> MetaConfig
  -> EitherT PackErr io MetaDB
loadDB mc = do
  when !(missing dbDir) updateDB
  debug "reading package collection"
  raw <- readFromTOML MetaDB dbFile
  case fileStem dbFile of
    Just "HEAD" => pure $ map toLatest raw
    _           => pure raw

||| Run a pack action in the directory of the cloned Idris repository.
export
withCoreGit :
     {auto _ : HasIO io}
  -> {auto e : Env}
  -> (Path Abs -> EitherT PackErr io a)
  -> EitherT PackErr io a
withCoreGit = withGit compiler e.db.idrisURL e.db.idrisCommit

||| Caches the `.ipkg` files of the core libraries to make them
||| quickly available when running queries.
export
cacheCoreIpkgFiles : HasIO io => Env => Path Abs -> EitherT PackErr io ()
cacheCoreIpkgFiles dir = for_ corePkgs $ \c =>
  copyFile (toAbsFile dir (coreIpkgPath c)) (coreCachePath c)

export
notCached : HasIO io => (e : Env) => PkgName -> Package -> io Bool
notCached n (Git u c i _ _) = fileMissing $ ipkgCachePath n c i
notCached n (Local d i _ _) = pure False
notCached n (Core c)        = fileMissing $ coreCachePath c

export
cachePkg :
     {auto _ : HasIO io}
  -> {auto e : Env}
  -> PkgName
  -> Package
  -> EitherT PackErr io ()
cachePkg n (Git u c i _ _) =
  let cache  := ipkgCachePath n c i
      tmpLoc := gitTmpDir n </> i
   in withGit n u c $ \dir => do
        let pf := patchFile n i
        when !(fileExists pf) (patch tmpLoc pf)
        copyFile tmpLoc cache
cachePkg n (Local d i _ _)    = pure ()
cachePkg n (Core c)           =
  let cache  := coreCachePath c
      tmpLoc := gitTmpDir compiler </> coreIpkgPath c
   in withCoreGit cacheCoreIpkgFiles

export
cachePkgs : HasIO io => (e : Env) => EitherT PackErr io ()
cachePkgs =
  let pkgs := toList allPackages
   in do
     (S n,ml,ps) <- needCaching Lin 0 60 pkgs | (0,_,_) => pure ()
     traverse_ (doCache (S n) ml) ps

  where
    needCaching :
         SnocList (Nat,PkgName,Package)
      -> (count : Nat)
      -> (maxLen : Nat)
      -> List (PkgName,Package)
      -> EitherT PackErr io (Nat,Nat,List (Nat,PkgName,Package))
    needCaching sp n ml []               = pure (n, ml, sp <>> [])
    needCaching sp n ml ((pn,pkg) :: ps) = do
      True <- notCached pn pkg | False => needCaching sp n ml ps
      let n'  := S n
          ml' := max ml (length (interpolate pn) + 26)
      needCaching (sp :< (n', pn, pkg)) n' ml' ps

    cacheInfo : (tot, maxLength, ix : Nat) -> PkgName -> String
    cacheInfo tot ml ix pn =
      let line := padRight ml '.' "Caching package info for \{pn} "
          stot := show tot
          six  := padLeft (length stot) ' ' (show ix)
       in "\{line} (\{six}/\{stot})"

    doCache :
          (tot : Nat)
       -> (maxLenght : Nat)
       -> (Nat,PkgName,Package)
       -> EitherT PackErr io ()
    doCache tot ml (n,pn,pkg) = do
      cache (cacheInfo tot ml n pn)
      cachePkg pn pkg

||| Load the package collection as given in the (auto-implicit) user config
||| and convert the result to a pack environment.
export covering
env :
     {auto _   : HasIO io}
  -> {auto pd  : PackDir}
  -> {auto td  : TmpDir}
  -> {auto ch  : LibCache}
  -> {auto lbf : LineBufferingCmd}
  -> (mc       : MetaConfig)
  -> (fetch    : Bool)
  -> EitherT PackErr io Env
env mc fetch = do
  mdb <- loadDB mc
  db  <- traverseDB (resolveMeta fetch) mdb
  c   <- traverse (resolveMeta fetch) db.idrisURL mc

  let url    := fromMaybe db.idrisURL c.idrisURL
      commit := fromMaybe db.idrisCommit c.idrisCommit
      -- take the DB's idris commit into account
      c'     := {allIdrisCommits $= (db.idrisCommit ::)} c
      -- adjust the idrisCommit and URL to use according to user overrides
      db'    := {idrisURL := url, idrisCommit := commit} db
      env    := MkEnv pd td c' ch db' lbf

  cachePkgs $> env

adjCollection : DBName -> String -> String
adjCollection db str = case isPrefixOf "collection " str of
  False => str
  True  => "collection = \{quote db}"

||| Update the `collection` field in file `PACK_DIR/user/pack.toml`
||| with the name of the package collection given in config `c`.
export covering
writeCollection :
     {auto _ : HasIO io}
  -> {auto _ : PackDir}
  -> {auto c : Config}
  -> EitherT PackErr io ()
writeCollection = do
  str <- read globalPackToml
  write globalPackToml (unlines . map (adjCollection c.collection) $ lines str)
