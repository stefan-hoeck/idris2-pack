module Pack.Config.Types

import Data.List
import Data.Maybe
import Data.SortedMap
import Data.String
import Idris.Package.Types
import Libraries.Data.List.Extra
import Libraries.Utils.Path
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

  ||| List detailed information about a package
  Details      : QueryType

||| Code generator to use
public export
data Codegen : Type where
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
Show Codegen where
  show Chez       = "chez"
  show ChezSep    = "chez-sep"
  show Racket     = "racket"
  show Gambit     = "gambit"
  show Node       = "node"
  show JavaScript = "javascript"
  show RefC       = "refc"
  show VMCode     = "vmcode-interp"
  show (Other x)  = x

export
Interpolation Codegen where interpolate = show

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
record Config_ (f : Type -> Type) (s : Maybe State) where
  constructor MkConfig
  ||| Directory where the *pack* DB and installed
  ||| libraries and executables reside
  packDir      : f Path

  ||| Package collection to use
  collection   : f DBName

  ||| Scheme executable to use
  scheme       : f Path

  ||| Whether to use bootstrapping when building Idris2
  bootstrap    : f Bool

  ||| Whether to prompt for a confirmation when
  ||| building or installing a package with custom
  ||| build or install hooks.
  safetyPrompt : f Bool

  ||| Whether to install the library sources as well
  withSrc      : f Bool

  ||| The `.ipkg` file to use when starting a REPL session
  withIpkg     : f (Maybe Path)

  ||| Whether to use `rlwrap` to run a REPL session
  rlwrap       : f Bool

  ||| Libraries to install automatically
  autoLibs     : f (List PkgName)

  ||| Applications to install automatically
  autoApps     : f (List PkgName)

  ||| Customizations to the package data base
  custom       : f (SortedMap DBName (SortedMap PkgName Package))

  ||| Type of query to run
  queryType    : f (QueryType)

  ||| Verbosity of the Log
  logLevel     : f (LogLevel)

  ||| Codegen to use
  codegen      : f (Codegen)

  ||| The package collection
  db           : f (DBType s)

--------------------------------------------------------------------------------
--          Updating the Config
--------------------------------------------------------------------------------

public export
0 Config : Maybe State -> Type
Config = Config_ I

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
init : (dir : Path) -> (coll : DBName) -> Config_ I Nothing
init dir coll = MkConfig {
    packDir      = dir
  , collection   = coll
  , scheme       = parse "scheme"
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
  , codegen      = Chez
  , db           = ()
  }

infixl 7 `update`

||| Update a config with optional settings
export
update : Config_ I Nothing -> Config_ Maybe Nothing -> Config_ I Nothing
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
tmpDir_ : (packDir : Path) -> Path
tmpDir_ packDir = packDir /> "tmp"

||| Temporary directory used for building packages.
export
tmpDir : Config s -> Path
tmpDir = tmpDir_ . packDir

||| Directory where databases are stored.
export
dbDir_ : (packDir : Path) -> Path
dbDir_ packDir = packDir /> "db"

||| Directory where databases are stored.
export
dbDir : Config s -> Path
dbDir = dbDir_ . packDir

||| Directory where databases are stored.
export
cacheDir : Config s -> Path
cacheDir c = c.packDir /> ".cache"

||| Path to cached `.ipkg` file.
export
ipkgPath : Config s -> PkgName -> Commit -> Path -> Path
ipkgPath c p com ipkg = cacheDir c /> p.value /> com.value /> show ipkg

||| Directory where user settings are stored.
export
userDir : Config s -> Path
userDir c = c.packDir /> "user"

export
packToml : (dir : Path) -> Path
packToml dir = dir /> "user" /> "pack.toml"

||| File where package DB is located
export
dbFile : Config s -> Path
dbFile c = dbDir c /> c.collection.value <.> "toml"

||| A symbolic link to `idrisBinDir` of the current
||| db version. This corresponds to `$PACK_DIR/bin`
||| and should be added to the `$PATH` variable in
||| order to have access to the current Idris2 binary
||| and related applications.
export
packBinDir : Config s -> Path
packBinDir c = c.packDir /> "bin"

||| Directory where symbolic links to installed binaries
||| will be placed.
export
collectionBinDir : Config s -> Path
collectionBinDir c = c.packDir /> c.collection.value /> "bin"

||| Symbolic link to the Idris2 executable associated with
||| each collection.
export
collectionIdrisExec : Config s -> Path
collectionIdrisExec c = collectionBinDir c /> "idris2"

||| Symbolic link to an application installed for a package
||| collection.
export
collectionAppExec : Config s -> String -> Path
collectionAppExec c s = collectionBinDir c /> s

||| `$SCHEME` variable during Idris2 installation
export
schemeVar : Config s -> String
schemeVar c = "SCHEME=\"\{show c.scheme}\""

||| Directory where `.ipkg` patches are stored.
export
patchesDir : Config s -> Path
patchesDir c = dbDir c /> "patches"

||| File where the patch (if any) for an `.ipkg` file is stored.
export
patchFile : Config s -> PkgName -> Path -> Path
patchFile c n ipkg =
  patchesDir c /> c.collection.value /> n.value /> "\{show ipkg}.patch"

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

||| Directory where all packages (and Idris2) built with the
||| current Idris2 commit will be installed.
export
commitDir : Env s -> Path
commitDir e = e.packDir /> "install" /> e.db.idrisCommit.value

||| The directory where Idris2 and core libraries will be installed.
export
idrisPrefixDir : Env s -> Path
idrisPrefixDir e = commitDir e /> "idris2"

||| The directory where the Idris2 binary will be installed.
export
idrisBinDir : Env s -> Path
idrisBinDir c = idrisPrefixDir c /> "bin"

||| Location of the Idris2 executable used to build
||| packages.
export
idrisExec : Env s -> Path
idrisExec c = idrisBinDir c /> "idris2"

||| `$PREFIX` variable during Idris2 installation
export
prefixVar : Env s -> String
prefixVar c = "PREFIX=\"\{show $ idrisPrefixDir c}\""

||| `$IDRIS2_BOOT` variable during Idris2 installation
export
idrisBootVar : Env s -> String
idrisBootVar c = "IDRIS2_BOOT=\"\{show $ idrisExec c}\""


idrisDir : Env e -> String
idrisDir e = "idris2-\{show e.db.idrisVersion}"

||| The directory where Idris2 packages will be installed.
export
idrisInstallDir : Env s -> Path
idrisInstallDir e = idrisPrefixDir e /> idrisDir e

||| The `lib` directory in the Idris2 installation folder
export
idrisLibDir : Env s -> Path
idrisLibDir e = idrisInstallDir e /> "lib"

||| The `support` directory in the Idris2 installation folder
export
idrisDataDir : Env s -> Path
idrisDataDir e = idrisInstallDir e /> "support"

export
githubPkgPrefixDir : Env s -> PkgName -> Commit -> Path
githubPkgPrefixDir e n c = commitDir e /> n.value /> c.value

export
localPkgPrefixDir : Env s -> PkgName -> Path
localPkgPrefixDir e n = commitDir e /> "local" /> n.value

||| Returns the directory where a package for the current
||| package collection is installed.
export
packagePrefixDir : Env s -> ResolvedPackage -> Path
packagePrefixDir e (RGitHub n _ c _ _) = githubPkgPrefixDir e n c
packagePrefixDir e (RIpkg _ d)         = idrisPrefixDir e
packagePrefixDir e (RLocal n _ _ _)    = localPkgPrefixDir e n
packagePrefixDir e Base                = idrisPrefixDir e
packagePrefixDir e Contrib             = idrisPrefixDir e
packagePrefixDir e Idris2              = idrisPrefixDir e
packagePrefixDir e Linear              = idrisPrefixDir e
packagePrefixDir e Network             = idrisPrefixDir e
packagePrefixDir e Prelude             = idrisPrefixDir e
packagePrefixDir e Test                = idrisPrefixDir e

export
packageInstallPrefix : Env s -> ResolvedPackage -> String
packageInstallPrefix e rp = "IDRIS2_PREFIX=\"\{show $ packagePrefixDir e rp}\""

||| Returns the directory where a package for the current
||| package collection is installed.
export
packageInstallDir : Env s -> ResolvedPackage -> Path
packageInstallDir e p =
  let vers = show $ e.db.idrisVersion
      dir  = packagePrefixDir e p /> idrisDir e
   in case p of
        Base     => dir /> "base-\{vers}"
        Contrib  => dir /> "contrib-\{vers}"
        Linear   => dir /> "linear-\{vers}"
        Network  => dir /> "network-\{vers}"
        Prelude  => dir /> "prelude-\{vers}"
        Idris2   => dir /> "idris2-\{vers}"
        Test     => dir /> "test-\{vers}"
        RGitHub _ _ _ _ d   =>
          let v = maybe "0" show d.version
           in dir /> "\{d.name}-\{v}"
        RIpkg p d =>
          let v = maybe "0" show d.version
           in dir /> "\{d.name}-\{v}"
        RLocal _ _ _ d =>
          let v = maybe "0" show d.version
           in dir /> "\{d.name}-\{v}"

||| Directory where an installed executable can be found
export
packageBinDir : Env s -> ResolvedPackage -> Path
packageBinDir e rp = packagePrefixDir e rp /> "bin"

||| Location of an executable of the given name.
export
packageExec : Env s -> ResolvedPackage -> String -> Path
packageExec e rp n = packageBinDir e rp /> n

||| `_app` directory of an executable of the given name.
export
packageAppDir : Env s -> ResolvedPackage -> String -> Path
packageAppDir e rp n = packageBinDir e rp /> "\{n}_app"

export
pkgPathDir : Env s -> (PkgName, Package) -> Path
pkgPathDir e (n, GitHub _ c _) = githubPkgPrefixDir e n c /> idrisDir e
pkgPathDir e (n, Local _ _)    = localPkgPrefixDir e n /> idrisDir e

export
pkgLibDir : Env s -> (PkgName, Package) -> Path
pkgLibDir e p = pkgPathDir e p /> "lib"

export
pkgDataDir : Env s -> (PkgName, Package) -> Path
pkgDataDir e p = pkgPathDir e p /> "support"

export
packagePathDirs : Env s -> String
packagePathDirs e =
  let pth = fastConcat
          . intersperse ":"
          . map (show . pkgPathDir e)
          $ SortedMap.toList (allPackages e)
   in "\{show $ idrisInstallDir e}:\{pth}"

export
packageLibDirs : Env s -> String
packageLibDirs e =
  let pth = fastConcat
          . intersperse ":"
          . map (show . pkgLibDir e)
          $ SortedMap.toList (allPackages e)
   in "\{show $ idrisLibDir e}:\{pth}"

export
packageDataDirs : Env s -> String
packageDataDirs e =
  let pth = fastConcat
          . intersperse ":"
          . map (show . pkgDataDir e)
          $ SortedMap.toList (allPackages e)
   in "\{show $ idrisDataDir e}:\{pth}"

export
packagePath : Env s -> String
packagePath e = "IDRIS2_PACKAGE_PATH=\"\{packagePathDirs e}\""

export
libPath : Env s -> String
libPath e = "IDRIS2_LIBS=\"\{packageLibDirs e}\""

export
dataPath : Env s -> String
dataPath e = "IDRIS2_DATA=\"\{packageDataDirs e}\""

export
buildEnv : Env s -> String
buildEnv e = "\{packagePath e} \{libPath e} \{dataPath e}"
