module Pack.CmdLn.Types

%default total

||| Commands accepted by *pack*. Most of these
||| operate on a list of packages and/or
||| projects with an `.ipkg` file.
public export
data Cmd : Type where
  Build          : String -> Cmd
  CheckDB        : String -> Cmd
  Exec           : String -> List String -> Cmd
  FromHEAD       : String -> Cmd
  Install        : List String -> Cmd
  InstallApp     : List String -> Cmd
  InstallWithSrc : List String -> Cmd
  PrintHelp      : Cmd
  Remove         : List String -> Cmd
  SwitchRepo     : String -> Cmd
  Typecheck      : String -> Cmd
  UpdateDB       : Cmd

||| Program configuration
public export
record Config where
  constructor MkConfig

  ||| Directory where the *pack* DB and installed
  ||| libraries and executables reside
  packDir   : String

  ||| Database to use
  dbVersion : String

  ||| Database to use
  scheme    : String

  ||| The pack command plus arguments to run
  cmd       : Cmd

||| Initial configuration.
export
init : (dir : String) -> (db : String) -> Config
init dir db = MkConfig {
    cmd           = PrintHelp
  , packDir       = dir
  , dbVersion     = db
  , scheme        = "scheme"
  }

||| Temporary directory used for building packages.
export
tmpDir : Config -> String
tmpDir c = "\{c.packDir}/tmp"

||| Directory where databases are stored.
export
dbDir : Config -> String
dbDir c = "\{c.packDir}/db"

||| Directory where user settings are stored.
export
userDir : Config -> String
userDir c = "\{c.packDir}/user"

||| File where a user-defined DB for the current
||| package collection might be stored.
export
userDB : Config -> String
userDB c = "\{userDir c}/\{c.dbVersion}.db"

||| File where a user-defined DB to be used in all
||| package collections might be stored.
export
userGlobalDB : Config -> String
userGlobalDB c = "\{userDir c}/global.db"

||| File where package DB is located
export
dbFile : Config -> String
dbFile c = "\{dbDir c}/\{c.dbVersion}.db"
