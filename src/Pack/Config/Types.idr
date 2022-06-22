module Pack.Config.Types

import Data.List
import Data.Maybe
import Data.SortedMap
import Idris.Package.Types
import Libraries.Data.List.Extra
import Pack.Core.Types
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

  ||| Whether to use bootstrapping when building Idris2
  bootstrap    : f Bool

  ||| Whether to prompt for a confirmation when
  ||| building or installing a package with custom
  ||| build or install hooks.
  safetyPrompt : f Bool

  ||| Whether to install the library sources as well
  withSrc      : f Bool

  ||| The `.ipkg` file to use when starting a REPL session
  withIpkg     : f (Maybe $ Path Abs)

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

||| Merges the "official" package collection with user
||| defined settings, which will take precedence.
export
allPackages : Env e -> SortedMap PkgName Package
allPackages e =
  let all = fromMaybe empty $ lookup "all" e.custom
      loc = fromMaybe empty $ lookup e.collection e.custom
   in e.db.packages `mergeRight` all `mergeRight` loc

||| Initial config
export
init : (dir : Path Abs) -> (coll : DBName) -> Config_ Commit I Nothing
init dir coll = MkConfig {
    packDir      = dir
  , collection   = coll
  , scheme       = "scheme"
  , bootstrap    = False
  , safetyPrompt = True
  , withSrc      = False
  , withIpkg     = Nothing
  , rlwrap       = False
  , autoLibs     = []
  , autoApps     = []
  , custom       = empty
  , queryType    = NameOnly
  , logLevel     = Info
  , codegen      = Default
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
  , bootstrap    = fromMaybe ci.bootstrap cm.bootstrap
  , safetyPrompt = fromMaybe ci.safetyPrompt cm.safetyPrompt
  , withSrc      = fromMaybe ci.withSrc cm.withSrc
  , withIpkg     = fromMaybe ci.withIpkg cm.withIpkg
  , rlwrap       = fromMaybe ci.rlwrap cm.rlwrap
  , autoLibs     = sortedNub (ci.autoLibs ++ concat cm.autoLibs)
  , autoApps     = sortedNub (ci.autoApps ++ concat cm.autoApps)
  , custom       = mergeWith mergeRight ci.custom (fromMaybe empty cm.custom)
  , queryType    = fromMaybe ci.queryType cm.queryType
  , logLevel     = fromMaybe ci.logLevel cm.logLevel
  , codegen      = fromMaybe ci.codegen cm.codegen
  , db           = ()
  }

--------------------------------------------------------------------------------
--          Files and Directories
--------------------------------------------------------------------------------

||| Temporary directory used for building packages.
export
tmpDir_ : (packDir : Path Abs) -> Path Abs
tmpDir_ packDir = packDir /> "tmp"

||| Temporary directory used for building packages.
export
tmpDir : Config s -> Path Abs
tmpDir = tmpDir_ . packDir

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
ipkgPath : Config s -> PkgName -> Commit -> Path Rel -> Path Abs
ipkgPath c p com ipkg = cacheDir c <//> p <//> com </> ipkg

-- path to cached core library `.ipkg` file
coreCachePath : Env s -> CorePkg -> Path Abs
coreCachePath  e n =
  cacheDir e <//> n <//> e.db.idrisCommit /> coreIpkgFile n

||| Path to cached `prelude.ipkg` file.
export
preludePath : Env s -> Path Abs
preludePath e = coreCachePath e Prelude

||| Path to cached `base.ipkg` file.
export
basePath : Env s -> Path Abs
basePath e = coreCachePath e Base

||| Path to cached `contrib.ipkg` file.
export
contribPath : Env s -> Path Abs
contribPath e = coreCachePath e Contrib

||| Path to cached `network.ipkg` file.
export
networkPath : Env s -> Path Abs
networkPath e = coreCachePath e Network

||| Path to cached `test.ipkg` file.
export
testPath : Env s -> Path Abs
testPath e = coreCachePath e Test

||| Path to cached `test.ipkg` file.
export
linearPath : Env s -> Path Abs
linearPath e = coreCachePath e Linear

||| Path to cached `idris2api.ipkg` file.
export
idrisApiPath : Env s -> Path Abs
idrisApiPath e = coreCachePath e IdrisApi

||| Directory where user settings are stored.
export
userDir : Config s -> Path Abs
userDir c = c.packDir /> "user"

export
packToml : (dir : Path Abs) -> Path Abs
packToml dir = dir </> "user/pack.toml"

||| File where package DB is located
export
dbFile : Config s -> Path Abs
dbFile c = (dbDir c <//> c.collection) <.> "toml"

||| A symbolic link to `idrisBinDir` of the current
||| db version. This corresponds to `$PACK_DIR/bin`
||| and should be added to the `$PATH` variable in
||| order to have access to the current Idris2 binary
||| and related applications.
export
packBinDir : Config s -> Path Abs
packBinDir c = c.packDir /> "bin"

||| Directory where symbolic links to installed binaries
||| will be placed.
export
collectionBinDir : Config s -> Path Abs
collectionBinDir c = c.packDir <//> c.collection /> "bin"

||| Symbolic link to the Idris2 executable associated with
||| each collection.
export
collectionIdrisExec : Config s -> Path Abs
collectionIdrisExec c = collectionBinDir c /> "idris2"

||| Symbolic link to an application installed for a package
||| collection.
export
collectionAppExec : Config s -> Path Rel -> Path Abs
collectionAppExec c s = collectionBinDir c </> s

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
patchFile : Config s -> PkgName -> Path Rel -> Path Abs
patchFile c n ipkg =
  patchesDir c <//> c.collection <//> n </> (ipkg <.> "patch")

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
idrisExec : Env s -> Path Abs
idrisExec c = idrisBinDir c /> "idris2"

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
githubPkgPrefixDir : Env s -> PkgName -> Commit -> Path Abs
githubPkgPrefixDir e n c = commitDir e <//> n <//> c

export
localPkgPrefixDir : Env s -> PkgName -> Path Abs
localPkgPrefixDir e n = commitDir e </> "local" <//> n

||| Returns the directory where a package for the current
||| package collection is installed.
export
packagePrefixDir : Env s -> ResolvedPackage -> Path Abs
packagePrefixDir e (RGitHub n _ c _ _ _) = githubPkgPrefixDir e n c
packagePrefixDir e (RLocal n _ _ _ _)    = localPkgPrefixDir e n
packagePrefixDir e (Core _ _)            = idrisPrefixDir e

export
packageInstallPrefix : Env s -> ResolvedPackage -> List (String,String)
packageInstallPrefix e rp = [("IDRIS2_PREFIX", "\{packagePrefixDir e rp}")]

pkgRelDir : PkgDesc -> Path Rel
pkgRelDir d = case body d.name of
  Just b  => neutral /> (b <-> d.version)
  Nothing => relPath d.name //> d.version

||| Returns the directory where a package for the current
||| package collection is installed.
export
packageInstallDir : Env s -> ResolvedPackage -> Path Abs
packageInstallDir e p =
  let vers = e.db.idrisVersion
      dir  = packagePrefixDir e p /> idrisDir e
   in case p of
        Core c _            => dir /> (c <-> vers)
        RGitHub _ _ _ _ _ d => dir </> pkgRelDir d
        RLocal _ _ _ _ d    => dir </> pkgRelDir d

||| Directory where an installed executable can be found
export
packageBinDir : Env s -> ResolvedPackage -> Path Abs
packageBinDir e rp = packagePrefixDir e rp /> "bin"

||| Location of an executable of the given name.
export
packageExec : Env s -> ResolvedPackage -> String -> Path Abs
packageExec e rp n = packageBinDir e rp <//> n

||| `_app` directory of an executable of the given name.
export
packageAppDir : Env s -> ResolvedPackage -> String -> Path Abs
packageAppDir e rp n = packageBinDir e rp <//> "\{n}_app"

export
pkgPrefixDir : Env s -> (PkgName, Package) -> Path Abs
pkgPrefixDir e (n, GitHub _ c _ _) = githubPkgPrefixDir e n c
pkgPrefixDir e (n, Local _ _ _)    = localPkgPrefixDir e n

export
pkgPathDir : Env s -> (PkgName, Package) -> Path Abs
pkgPathDir e p = pkgPrefixDir e p /> idrisDir e

export
pkgBinDir : Env s -> (PkgName, Package) -> Path Abs
pkgBinDir e p = pkgPrefixDir e p /> "bin"

export
pkgLibDir : Env s -> (PkgName, Package) -> Path Abs
pkgLibDir e p = pkgPathDir e p /> "lib"

export
pkgDataDir : Env s -> (PkgName, Package) -> Path Abs
pkgDataDir e p = pkgPathDir e p /> "support"

export
packagePathDirs : Env s -> String
packagePathDirs e =
  let pth = fastConcat
          . intersperse ":"
          . map (show . pkgPathDir e)
          $ SortedMap.toList (allPackages e)
   in "\{idrisInstallDir e}:\{pth}"

export
packageLibDirs : Env s -> String
packageLibDirs e =
  let pth = fastConcat
          . intersperse ":"
          . map (show . pkgLibDir e)
          $ SortedMap.toList (allPackages e)
   in "\{idrisLibDir e}:\{pth}"

export
packageDataDirs : Env s -> String
packageDataDirs e =
  let pth = fastConcat
          . intersperse ":"
          . map (show . pkgDataDir e)
          $ SortedMap.toList (allPackages e)
   in "\{idrisDataDir e}:\{pth}"

export
packagePath : Env s -> (String, String)
packagePath e = ("IDRIS2_PACKAGE_PATH", packagePathDirs e)

export
libPath : Env s -> (String, String)
libPath e = ("IDRIS2_LIBS", packageLibDirs e)

export
dataPath : Env s -> (String, String)
dataPath e = ("IDRIS2_DATA", packageDataDirs e)

export
buildEnv : Env s -> List (String,String)
buildEnv e = [packagePath e, libPath e, dataPath e]

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
idrisWithPkg :  Env HasIdris
             -> ResolvedPackage
             -> (String, List (String,String))
idrisWithPkg e rp =
  ("\{idrisWithCG e} -p \{name rp}", buildEnv e)

||| Idris executable loading the given packages plus the
||| environment variables needed to run it.
export
idrisWithPkgs :  Env HasIdris
              -> List ResolvedPackage
              -> (String, List (String,String))
idrisWithPkgs e [] = (idrisWithCG e, [])
idrisWithPkgs e pkgs =
  let ps = fastConcat $ map (\p => " -p \{name p}") pkgs
   in ("\{idrisWithCG e}\{ps}", buildEnv e)
