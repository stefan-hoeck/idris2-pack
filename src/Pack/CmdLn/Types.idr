module Pack.CmdLn.Types

import Data.List1
import Data.String
import Idris.Package.Types
import Pack.Core.Types
import Pack.Database.Types
import Libraries.Utils.Path

%default total

||| Commands accepted by *pack*. Most of these
||| operate on a list of packages and/or
||| projects with an `.ipkg` file.
public export
data Cmd : Type where
  Build            : Path -> Cmd
  Typecheck        : Path -> Cmd
  Exec             : PkgRep -> List String -> Cmd

  Install          : List PkgRep -> Cmd
  InstallApp       : List PkgRep -> Cmd
  InstallWithSrc   : List PkgRep -> Cmd
  Remove           : List PkgRep -> Cmd

  CheckDB          : DBName -> Cmd
  FromHEAD         : Path -> Cmd
  SwitchRepo       : DBName -> Cmd
  UpdateDB         : Cmd

  Completion       : String -> String -> Cmd
  CompletionScript : String -> Cmd

  PrintHelp        : Cmd

||| Program configuration
public export
record Config where
  constructor MkConfig

  ||| Directory where the *pack* DB and installed
  ||| libraries and executables reside
  packDir   : Path

  ||| Database to use
  dbVersion : DBName

  ||| Scheme executable to use
  scheme    : Path

  ||| The pack command plus arguments to run
  cmd       : Cmd

||| Initial configuration.
export
init : (dir : Path) -> (db : DBName) -> Config
init dir db = MkConfig {
    cmd           = PrintHelp
  , packDir       = dir
  , dbVersion     = db
  , scheme        = parse "scheme"
  }

||| Temporary directory used for building packages.
export
tmpDir : Config -> Path
tmpDir c = c.packDir /> "tmp"

||| Directory where databases are stored.
export
dbDir : Config -> Path
dbDir c = c.packDir /> "db"

||| Directory where databases are stored.
export
cacheDir : Config -> Path
cacheDir c = c.packDir /> ".cache"

||| Path to cached `.ipkg` file.
export
ipkgPath : Config -> PkgName -> Commit -> Path -> Path
ipkgPath c p com ipkg = cacheDir c /> p.value /> com.value /> show ipkg

||| Directory where user settings are stored.
export
userDir : Config -> Path
userDir c = c.packDir /> "user"

||| File where a user-defined DB for the current
||| package collection might be stored.
export
userDB : Config -> Path
userDB c = userDir c /> c.dbVersion.value <.> "db"

||| File where a user-defined DB to be used in all
||| package collections might be stored.
export
userGlobalDB : Config -> Path
userGlobalDB c = userDir c /> "global.db"

||| File where package DB is located
export
dbFile : Config -> Path
dbFile c = dbDir c /> c.dbVersion.value <.> "db"

||| The directory where Idris2, installed libraries,
||| and binaries will be installed.
|||
||| This corresponds to "$IDRIS2_PREFIX".
export
idrisPrefixDir : Config -> Path
idrisPrefixDir c = c.packDir /> c.dbVersion.value

||| The directory where binaries will be installed.
export
idrisBinDir : Config -> Path
idrisBinDir c = idrisPrefixDir c /> "bin"

||| A symbolic link to `idrisBinDir` of the current
||| db version. This corresponds to `$PACK_DIR/bin`
||| and should be added to the `$PATH` variable in
||| order to have access to the current Idris2 binary
||| and related applications.
export
packBinDir : Config -> Path
packBinDir c = c.packDir /> "bin"

||| A symbolic to `idrisInstallDir` of the current
||| db version. Let `$IDRIS2_PREFIX` point to this
||| directory.
export
packIdrisDir : Config -> Path
packIdrisDir c = c.packDir /> "idris2"

||| Location of the Idris2 executable used to build
||| packages.
export
idrisExec : Config -> Path
idrisExec c = idrisBinDir c /> "idris2"

||| Location of an executable of the given name.
export
packageExec : Config -> String -> Path
packageExec c n = idrisBinDir c /> n

||| `_app` directory of an executable of the given name.
export
packageAppDir : Config -> String -> Path
packageAppDir c n = idrisBinDir c /> "\{n}_app"

||| `$PREFIX` variable during Idris2 installation
export
prefixVar : Config -> String
prefixVar c = "PREFIX=\"\{show $ idrisPrefixDir c}\""

||| `$PREFIX` variable during Idris2 installation
export
idrisBootVar : Config -> String
idrisBootVar c = "IDRIS2_BOOT=\"\{show $ idrisExec c}\""

||| `$PREFIX` variable during Idris2 installation
export
schemeVar : Config -> String
schemeVar c = "SCHEME=\"\{show c.scheme}\""

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

||| State of the environment. This is to make sure that
||| certain necessary checks (if any) have been run.
public export
data State : Type where
  ||| No checks where run
  None : State

  ||| Idris installation has been verified
  HasIdris : State

||| Package environment consisting of the application
||| config and the package database.
|||
||| This is indexed over a state variable to make sure
||| that necessary precalculations have been run.
public export
record Env (st : State) where
  constructor MkEnv
  db    : DB
  conf  : Config

||| The directory where Idris2 packages will be installed.
export
idrisInstallDir : Env s -> Path
idrisInstallDir e = idrisPrefixDir e.conf /> "idris2-\{show e.db.idrisVersion}"

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
