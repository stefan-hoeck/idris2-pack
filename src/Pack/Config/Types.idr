module Pack.Config.Types

import Data.Maybe
import Data.SortedMap
import Data.String
import Idris.Package.Types
import Pack.Core.Types
import Pack.Database.Types
import Libraries.Utils.Path

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

||| Program configuration
public export
record Config (s : Maybe State) where
  constructor MkConfig
  ||| Directory where the *pack* DB and installed
  ||| libraries and executables reside
  packDir      : Path

  ||| Package collection to use
  collection   : DBName

  ||| Scheme executable to use
  scheme       : Path

  ||| Whether to use bootstrapping when building Idris2
  bootstrap    : Bool

  ||| Whether to prompt for a confirmation when
  ||| building or installing a package with custom
  ||| build or install hooks.
  safetyPrompt : Bool

  ||| True if symlinks in the `$HOME/.pack` dir should
  ||| be made to point to the currently used data collection's
  ||| installation directory
  switchDB     : Bool

  ||| Whether to install the library sources as well
  withSrc      : Bool

  ||| The `.ipkg` file to use when starting a REPL session
  withIpkg     : Maybe Path

  ||| Whether to use `rlwrap` to run a REPL session
  rlwrap       : Bool

  ||| Libraries to install automatically
  autoLibs     : List PkgName

  ||| Applications to install automatically
  autoApps     : List PkgName

  ||| Customizations to the package data base
  custom       : SortedMap DBName (SortedMap PkgName Package)

  ||| Type of query to run
  queryType    : QueryType

  ||| Verbosity of the Log
  logLevel     : LogLevel

  ||| The package collection
  db           : DBType s

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

||| Temporary directory used for building packages.
export
tmpDir : Config s -> Path
tmpDir c = c.packDir /> "tmp"

||| Directory where databases are stored.
export
dbDir : Config s -> Path
dbDir c = c.packDir /> "db"

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
