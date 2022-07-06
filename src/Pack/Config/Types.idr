module Pack.Config.Types

import Data.List
import Data.Maybe
import Data.SortedMap as SM
import Idris.Package.Types
import Libraries.Data.List.Extra
import Pack.Core
import Pack.Database.Types

%default total

||| State of the program configuration.
||| This is to make sure that
||| certain necessary checks (if any) have been run.
public export
data State : Type where
  ||| The data collection has been loaded
  DBLoaded : State

  ||| Idris installation has been verified
  HasIdris : State

||| The program configuration is indexed over `Maybe State`,
||| which is used to monitor if the data collection has been
||| loaded and whether Idris has be verified to be installed.
|||
||| Depending on this index, the `db` fields will be of
||| a different type.
public export
0 DBType : Maybe State -> Type
DBType (Just _) = DB
DBType Nothing  = ()

||| Results to show when querying the data base
public export
data QueryType : Type where
  ||| Display only the matching packages' names
  NameOnly     : QueryType

  ||| Display names and short descriptions
  ShortDesc    : QueryType

  ||| List direct dependencies
  Dependencies : QueryType

  ||| Print the full `.ipkg` file
  Ipkg         : QueryType

  ||| List detailed information about a package
  Details      : QueryType

||| Code generator to use
public export
data Codegen : Type where
  Default    : Codegen
  Chez       : Codegen
  ChezSep    : Codegen
  Racket     : Codegen
  Gambit     : Codegen
  Node       : Codegen
  JavaScript : Codegen
  RefC       : Codegen
  VMCode     : Codegen
  Other      : String -> Codegen

export
Interpolation Codegen where
  interpolate Default    = ""
  interpolate Chez       = "chez"
  interpolate ChezSep    = "chez-sep"
  interpolate Racket     = "racket"
  interpolate Gambit     = "gambit"
  interpolate Node       = "node"
  interpolate JavaScript = "javascript"
  interpolate RefC       = "refc"
  interpolate VMCode     = "vmcode-interp"
  interpolate (Other x)  = x

public export
fromString : String -> Codegen
fromString "chez"          = Chez
fromString "chez-sep"      = ChezSep
fromString "racket"        = Racket
fromString "gambit"        = Gambit
fromString "node"          = Node
fromString "javascript"    = JavaScript
fromString "refc"          = RefC
fromString "vmcode-interp" = VMCode
fromString x               = Other x

||| Data type describing whether to search for an `.ipkg`
||| file in one of the parent directories (the default), not
||| using an `.ipkg` file at all, or use the one provided at
||| the command line.
|||
||| This is only relevant when working with Idris source files
||| directly, for instance when loading them into a REPL session.
public export
data WithIpkg : Type where
  ||| Search for an `.ipkg` file in a parent directory.
  ||| If an `.idr` file is provided, the parent directories
  ||| of this file will be searched. If no `.idr` file is
  ||| given, the current working directory will be search,
  ||| which is given as an argument to this constructor.
  Search : (dir : Path Abs) -> WithIpkg

  ||| Don't use an `.ipkg` file.
  None   : WithIpkg

  ||| Use the given `.ipkg` file, provided as a command line
  ||| argument.
  Use    : (ipkg : File Abs) -> WithIpkg

||| Type-level identity
public export
0 I : Type -> Type
I t = t

||| Program configuration
|||
||| Parameter `f` is used to represent the context of values.
||| When we use the config as an environment for running *pack*
||| programs is, the context will be set to `I`. For updating
||| the configuration from user config file, we use context
||| `Maybe`, because all values will be optional.
public export
record Config_ (c : Type) (f : Type -> Type) (s : Maybe State) where
  constructor MkConfig
  ||| Directory where the *pack* DB and installed
  ||| libraries and executables reside
  packDir      : f (Path Abs)

  ||| Package collection to use
  collection   : f DBName

  ||| Scheme executable to use
  scheme       : f FilePath

  ||| Whether to prompt for a confirmation when
  ||| building or installing a package with custom
  ||| build or install hooks.
  safetyPrompt : f Bool

  ||| Whether to install the library sources as well
  withSrc      : f Bool

  ||| Whether to install the library docs as well
  withDocs      : f Bool

  ||| Whether to use katla to add semantically highlighted source code
  ||| to the library docs.
  useKatla      : f Bool

  ||| The `.ipkg` file to use (if any) when starting a REPL session
  withIpkg     : f WithIpkg

  ||| Whether to use `rlwrap` to run a REPL session
  rlwrap       : f Bool

  ||| Libraries to install automatically
  autoLibs     : f (List PkgName)

  ||| Applications to install automatically
  autoApps     : f (List PkgName)

  ||| Customizations to the package data base
  custom       : f (SortedMap DBName (SortedMap PkgName $ Package_ c))

  ||| Type of query to run
  queryType    : f (QueryType)

  ||| Verbosity of the Log
  logLevel     : f (LogLevel)

  ||| Codegen to use
  codegen      : f (Codegen)

  ||| Name of output file when compiling Idris source files
  output       : f Body

  ||| The package collection
  db           : f (DBType s)

export
traverse :  Applicative f
         => (Package_ a -> f (Package_ b))
         -> Config_ a Maybe s
         -> f (Config_ b Maybe s)
traverse f cfg =
  let cst = traverse (traverse (traverse f)) cfg.custom
   in map (\c => {custom := c} cfg) cst

--------------------------------------------------------------------------------
--          Updating the Config
--------------------------------------------------------------------------------

public export
0 Config : Maybe State -> Type
Config = Config_ Commit I

||| Program configuration with data collection
public export
0 Env : State -> Type
Env = Config . Just

infixl 8 `mergeRight`

mergeRight : SortedMap k v -> SortedMap k v -> SortedMap k v
mergeRight = mergeWith (\_,v => v)

pkgs : SortedMap PkgName Package
pkgs = fromList $ (\c => (corePkgName c, Core c)) <$> corePkgs

||| Merges the "official" package collection with user
||| defined settings, which will take precedence.
export
allPackages : Env e -> SortedMap PkgName Package
allPackages e =
  let all = fromMaybe empty $ lookup All e.custom
      loc = fromMaybe empty $ lookup e.collection e.custom
   in e.db.packages `mergeRight` all `mergeRight` loc `mergeRight` pkgs

||| Initial config
export
init :  (cur, dir : Path Abs)
     -> (coll : DBName)
     -> Config_ Commit I Nothing
init cur dir coll = MkConfig {
    packDir      = dir
  , collection   = coll
  , scheme       = "scheme"
  , safetyPrompt = True
  , withSrc      = False
  , withDocs     = False
  , useKatla     = False
  , withIpkg     = Search cur
  , rlwrap       = False
  , autoLibs     = []
  , autoApps     = []
  , custom       = empty
  , queryType    = NameOnly
  , logLevel     = Warning
  , codegen      = Default
  , output       = "_tmppack"
  , db           = ()
  }

infixl 7 `update`

||| Update a config with optional settings
export
update : Config_ c I Nothing -> Config_ c Maybe Nothing -> Config_ c I Nothing
update ci cm = MkConfig {
    packDir      = fromMaybe ci.packDir cm.packDir
  , collection   = fromMaybe ci.collection cm.collection
  , scheme       = fromMaybe ci.scheme cm.scheme
  , safetyPrompt = fromMaybe ci.safetyPrompt cm.safetyPrompt
  , withSrc      = fromMaybe ci.withSrc cm.withSrc
  , withDocs     = fromMaybe ci.withDocs cm.withDocs
  , useKatla     = fromMaybe ci.useKatla cm.useKatla
  , withIpkg     = fromMaybe ci.withIpkg cm.withIpkg
  , rlwrap       = fromMaybe ci.rlwrap cm.rlwrap
  , autoLibs     = sortedNub (ci.autoLibs ++ concat cm.autoLibs)
  , autoApps     = sortedNub (ci.autoApps ++ concat cm.autoApps)
  , custom       = mergeWith mergeRight ci.custom (fromMaybe empty cm.custom)
  , queryType    = fromMaybe ci.queryType cm.queryType
  , logLevel     = fromMaybe ci.logLevel cm.logLevel
  , codegen      = fromMaybe ci.codegen cm.codegen
  , output       = fromMaybe ci.output cm.output
  , db           = ()
  }

--------------------------------------------------------------------------------
--          Files and Directories
--------------------------------------------------------------------------------

export
packToml : Body
packToml = "pack.toml"

||| Temporary directory used for building packages.
export
tmpDir_ : (packDir : Path Abs) -> Path Abs
tmpDir_ packDir = packDir /> ".tmp"

||| Temporary directory used for building packages.
export
tmpDir : Config s -> Path Abs
tmpDir c = packDir c /> (".tmp-" <+> cast c.collection)

||| Clone of the pack GitHub repo
export
packClone : Config s -> Path Abs
packClone c = tmpDir c /> "pack"

||| Directory where databases are stored.
export
dbDir_ : (packDir : Path Abs) -> Path Abs
dbDir_ packDir = packDir /> "db"

||| Directory where databases are stored.
export
dbDir : Config s -> Path Abs
dbDir = dbDir_ . packDir

||| Directory where databases are stored.
export
cacheDir : Config s -> Path Abs
cacheDir c = c.packDir /> ".cache"

||| Path to cached `.ipkg` file.
export
ipkgCachePath : Config s -> PkgName -> Commit -> File Rel -> File Abs
ipkgCachePath c p com = toAbsFile (cacheDir c <//> p <//> com)

||| Path to cached core library `.ipkg` file
export
coreCachePath : Env s -> CorePkg -> File Abs
coreCachePath  e n =
  MkF (cacheDir e <//> n <//> e.db.idrisCommit) (coreIpkgFile n)

||| Directory where user settings are stored.
export
userDir : Config s -> Path Abs
userDir c = c.packDir /> "user"

||| File where package DB is located
export
dbFile : Config s -> File Abs
dbFile c = MkF (dbDir c) $ c.collection.value <+> ".toml"

||| Directory where wrapper scripts to binaries
||| managed by pack are being stored. The only exception
||| is pack itself, which is stored as a symbolic link.
export
packBinDir : Config s -> Path Abs
packBinDir c = c.packDir /> "bin"

export
packInstallDir : Config s -> Commit -> Path Abs
packInstallDir c com = c.packDir </> "install/pack" </> cast com

||| Executable for an application
export
pathExec : Config s -> Body -> File Abs
pathExec c b = MkF (packBinDir c) b

||| Symbolic link to the current pack executable.
export
packExec : Config s -> File Abs
packExec c = pathExec c "pack"

||| `$SCHEME` variable during Idris2 installation
export
schemeVar : Config s -> String
schemeVar c = "SCHEME=\"\{c.scheme}\""

||| Directory where `.ipkg` patches are stored.
export
patchesDir : Config s -> Path Abs
patchesDir c = dbDir c /> "patches"

||| File where the patch (if any) for an `.ipkg` file is stored.
export
patchFile : Config s -> PkgName -> File Rel -> File Abs
patchFile c n (MkF p b) = MkF
  (patchesDir c //> c.collection <//> n </> p)
  (b <+> ".patch")

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

||| Directory where all packages (and Idris2) built with the
||| current Idris2 commit will be installed.
export
commitDir : Env s -> Path Abs
commitDir e = e.packDir </> "install" <//> e.db.idrisCommit

||| The directory where Idris2 and core libraries will be installed.
export
idrisPrefixDir : Env s -> Path Abs
idrisPrefixDir e = commitDir e /> "idris2"

||| The directory where the Idris2 binary will be installed.
export
idrisBinDir : Env s -> Path Abs
idrisBinDir c = idrisPrefixDir c /> "bin"

||| Location of the Idris2 executable used to build
||| packages.
export
idrisExec : Env s -> File Abs
idrisExec c = MkF (idrisBinDir c) "idris2"

||| `$PREFIX` variable during Idris2 installation
export
prefixVar : Env s -> String
prefixVar c = "PREFIX=\"\{idrisPrefixDir c}\""

||| `$IDRIS2_BOOT` variable during Idris2 installation
export
idrisBootVar : Env s -> String
idrisBootVar c = "IDRIS2_BOOT=\"\{idrisExec c}\""

idrisDir : Env e -> Body
idrisDir e = the Body "idris2" <-> e.db.idrisVersion

||| The directory where Idris2 packages will be installed.
export
idrisInstallDir : Env s -> Path Abs
idrisInstallDir e = idrisPrefixDir e /> idrisDir e

||| The `lib` directory in the Idris2 installation folder
export
idrisLibDir : Env s -> Path Abs
idrisLibDir e = idrisInstallDir e /> "lib"

||| The `support` directory in the Idris2 installation folder
export
idrisDataDir : Env s -> Path Abs
idrisDataDir e = idrisInstallDir e /> "support"

export
pkgPrefixDir : Env s -> PkgName -> Package -> Path Abs
pkgPrefixDir e n (GitHub _ c _ _) = commitDir e <//> n <//> c
pkgPrefixDir e n (Local _ _ _)    = commitDir e </> "local" <//> n
pkgPrefixDir e n (Core _)         = idrisPrefixDir e

export %inline
pkgPathDir : Env s -> PkgName -> Package -> Path Abs
pkgPathDir e n p = pkgPrefixDir e n p /> idrisDir e

export %inline
pkgBinDir : Env s -> PkgName -> Package -> Path Abs
pkgBinDir e n p = pkgPrefixDir e n p /> "bin"

export %inline
pkgLibDir : Env s -> PkgName -> Package -> Path Abs
pkgLibDir e n p = pkgPathDir e n p /> "lib"

export %inline
pkgDataDir : Env s -> PkgName -> Package -> Path Abs
pkgDataDir e n p = pkgPathDir e n p /> "support"

||| Directory where the API docs of the package will be installed.
export %inline
pkgDocs : Env s -> PkgName -> Package -> Path Abs
pkgDocs e n p = pkgPrefixDir e n p /> "docs"

||| Timestamp used to monitor if a local library has been
||| modified and requires reinstalling.
export %inline
libTimestamp :  Env s -> PkgName -> Package -> File Abs
libTimestamp e n p = MkF (pkgPathDir e n p) ".timestamp"

||| Timestamp used to monitor if a local app has been
||| modified and requires reinstalling.
export %inline
appTimestamp :  Env s -> PkgName -> Package -> File Abs
appTimestamp e n p = MkF (pkgBinDir e n p) ".timestamp"

||| Directory where the sources of a local package are
||| stored.
export
localSrcDir : Desc t -> Path Abs
localSrcDir d = sourcePath d

pkgRelDir : Desc t -> Path Rel
pkgRelDir d = case Body.parse d.desc.name of
  Just b  => neutral /> (b <-> d.desc.version)
  Nothing => cast d.desc.name //> d.desc.version

||| Returns the directory where a package for the current
||| package collection is installed.
export
pkgInstallDir : Env s -> PkgName -> Package -> Desc t -> Path Abs
pkgInstallDir e n p d =
  let vers = e.db.idrisVersion
      dir  = pkgPrefixDir e n p /> idrisDir e
   in case p of
        Core c         => dir /> (c <-> vers)
        GitHub _ _ _ _ => dir </> pkgRelDir d
        Local _ _ _    => dir </> pkgRelDir d

||| Location of an executable of the given name.
export
pkgExec : Env s -> PkgName -> Package -> (exe : Body) -> File Abs
pkgExec e n p exe = MkF (pkgBinDir e n p) exe

export
resolvedExec : Env s -> ResolvedApp t -> File Abs
resolvedExec e (RA p n d _ exe) = pkgExec e n p exe

export
libInstallPrefix : Env s -> ResolvedLib t -> List (String,String)
libInstallPrefix e rl =
  [("IDRIS2_PREFIX", "\{pkgPrefixDir e rl.name rl.pkg}")]

pathDirs :  HasIO io
         => Env s
         -> (pre : String)
         -> (pth : Env s -> PkgName -> Package -> Path Abs)
         -> io String
pathDirs e pre pth = do
  ps <- filterM (\(n,p) => exists $ pth e n p) (SM.toList $ allPackages e)
  let ps' := filter (not . isCorePkg . value . fst) ps
  pure $ fastConcat
       . intersperse ":"
       . (pre ::)
       $ map (\(n,p) => "\{pth e n p}") ps'

export
packagePathDirs : HasIO io => Env s -> io String
packagePathDirs e = pathDirs e "\{idrisInstallDir e}" pkgPathDir

export
packageLibDirs : HasIO io => Env s -> io String
packageLibDirs e = pathDirs e "\{idrisLibDir e}" pkgLibDir

export
packageDataDirs : HasIO io => Env s -> io String
packageDataDirs e = pathDirs e "\{idrisDataDir e}" pkgDataDir

export
packagePath : HasIO io => Env s -> io (String, String)
packagePath e = ("IDRIS2_PACKAGE_PATH",) <$>  packagePathDirs e

export
libPath : HasIO io => Env s -> io (String, String)
libPath e = ("IDRIS2_LIBS",) <$> packageLibDirs e

export
dataPath : HasIO io => Env s -> io (String, String)
dataPath e = ("IDRIS2_DATA",) <$> packageDataDirs e

export
buildEnv : HasIO io => Env s -> io (List (String,String))
buildEnv e = sequence [packagePath e, libPath e, dataPath e]

||| Idris executable to use together with the
||| `--cg` (codegen) command line option.
export
idrisWithCG : Env HasIdris -> String
idrisWithCG e = case e.codegen of
  Default => "\{idrisExec e}"
  cg      => "\{idrisExec e} --cg \{cg}"

||| Idris executable loading the given package plus the
||| environment variables needed to run it.
export
idrisWithPkg :  HasIO io
             => Env HasIdris
             -> ResolvedLib t
             -> io (String, List (String,String))
idrisWithPkg e rl =
  ("\{idrisWithCG e} -p \{name rl}",) <$> buildEnv e

||| Idris executable loading the given packages plus the
||| environment variables needed to run it.
export
idrisWithPkgs :  HasIO io
              => Env HasIdris
              -> List (ResolvedLib t)
              -> io (String, List (String,String))
idrisWithPkgs e [] = pure (idrisWithCG e, [])
idrisWithPkgs e pkgs =
  let ps = fastConcat $ map (\p => " -p \{name p}") pkgs
   in ("\{idrisWithCG e}\{ps}",) <$> buildEnv e
