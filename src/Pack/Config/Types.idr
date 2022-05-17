module Pack.Config.Types

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

  ||| True if symlinks in the `$HOME/.pack` dir should
  ||| be made to point to the currently used data collection's
  ||| installation directory
  switchDB     : f Bool

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
  , switchDB     = False
  , withSrc      = False
  , withIpkg     = Nothing
  , rlwrap       = False
  , autoLibs     = []
  , autoApps     = []
  , custom       = empty
  , queryType    = NameOnly
  , logLevel     = Info
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
  , switchDB     = fromMaybe ci.switchDB cm.switchDB
  , withSrc      = fromMaybe ci.withSrc cm.withSrc
  , withIpkg     = fromMaybe ci.withIpkg cm.withIpkg
  , rlwrap       = fromMaybe ci.rlwrap cm.rlwrap
  , autoLibs     = sortedNub (ci.autoLibs ++ concat cm.autoLibs)
  , autoApps     = sortedNub (ci.autoApps ++ concat cm.autoApps)
  , custom       = mergeWith mergeRight ci.custom (fromMaybe empty cm.custom)
  , queryType    = fromMaybe ci.queryType cm.queryType
  , logLevel     = fromMaybe ci.logLevel cm.logLevel
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

||| The directory where Idris2, installed libraries,
||| and binaries will be installed.
|||
||| This corresponds to "$IDRIS2_PREFIX".
export
idrisPrefixDir : Config s -> Path
idrisPrefixDir c = c.packDir /> c.collection.value

||| The directory where binaries will be installed.
export
idrisBinDir : Config s -> Path
idrisBinDir c = idrisPrefixDir c /> "bin"

||| A symbolic link to `idrisBinDir` of the current
||| db version. This corresponds to `$PACK_DIR/bin`
||| and should be added to the `$PATH` variable in
||| order to have access to the current Idris2 binary
||| and related applications.
export
packBinDir : Config s -> Path
packBinDir c = c.packDir /> "bin"

||| A symbolic to `idrisInstallDir` of the current
||| db version.
export
packIdrisDir : Config s -> Path
packIdrisDir c = c.packDir /> "idris2"

||| Location of the Idris2 executable used to build
||| packages.
export
idrisExec : Config s -> Path
idrisExec c = idrisBinDir c /> "idris2"

||| Location of an executable of the given name.
export
packageExec : Config s -> String -> Path
packageExec c n = idrisBinDir c /> n

||| `_app` directory of an executable of the given name.
export
packageAppDir : Config s -> String -> Path
packageAppDir c n = idrisBinDir c /> "\{n}_app"

||| `$PREFIX` variable during Idris2 installation
export
prefixVar : Config s -> String
prefixVar c = "PREFIX=\"\{show $ idrisPrefixDir c}\""

||| `$PREFIX` variable during Idris2 installation
export
idrisBootVar : Config s -> String
idrisBootVar c = "IDRIS2_BOOT=\"\{show $ idrisExec c}\""

||| `$PREFIX` variable during Idris2 installation
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

||| The directory where Idris2 packages will be installed.
export
idrisInstallDir : Env s -> Path
idrisInstallDir e = idrisPrefixDir e /> "idris2-\{show e.db.idrisVersion}"

||| Returns the directory where a package for the current
||| package collection is installed.
export
packageInstallDir : Env s -> ResolvedPackage -> Path
packageInstallDir e p =
  let vers = show $ e.db.idrisVersion
      dir  = idrisInstallDir e
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
