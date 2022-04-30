module Pack.CmdLn

import Pack.Err
import System.Console.GetOpt

%default total

||| Commands accepted by *pack*. Most of these
||| operate on a list of packages and/or
||| projects with an `.ipkg` file.
public export
data Cmd : Type where
  Build          : String -> Cmd
  CheckDB        : String -> Cmd
  Exec           : String -> List String -> Cmd
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

--------------------------------------------------------------------------------
--          Applying Command Line Args
--------------------------------------------------------------------------------

dir : String -> Config -> Config
dir s = {packDir := s}

setDB : String -> Config -> Config
setDB s = {dbVersion := s}

setScheme : String -> Config -> Config
setScheme s = {scheme := s}

-- command line options with description
descs : List $ OptDescr (Config -> Config)
descs = [ MkOpt [] ["pack-dir"]   (ReqArg dir "<dir>")
            """
            Directory where pack stores its database and
            installed packages. This defaults to \"$PACK_DIR\"
            (if set) or \"$HOME/.pack\" otherwise.
            """
        , MkOpt ['p'] ["package-set"]   (ReqArg setDB "<db>")
            """
            Set the curated package set to use. At the
            moment, this defaults to `HEAD`, so the latest commits
            of all packages will be used. This is bound to change
            once we have a reasonably stable package set.
            """
        , MkOpt ['s'] ["scheme"]   (ReqArg setScheme "<scheme executable>")
            """
            Sets the scheme executable for installing the Idris2 compiler.
            As a default, this is set to `scheme`.
            """
        ]

cmd : List String -> Either PackErr Cmd
cmd []                         = Right PrintHelp
cmd ["help"]                   = Right PrintHelp
cmd ["update-db"]              = Right UpdateDB
cmd ["check-db", db]           = Right $ CheckDB db
cmd ("exec" :: file :: args)   = Right $ Exec file args
cmd ["build", file]            = Right $ Build file
cmd ["typecheck", file]        = Right $ Typecheck file
cmd ["switch", repo]           = Right $ SwitchRepo repo
cmd ("install" :: xs)          = Right $ Install xs
cmd ("remove" :: xs)           = Right $ Remove xs
cmd ("install-with-src" :: xs) = Right $ InstallWithSrc xs
cmd ("install-app" :: xs)      = Right $ InstallApp xs
cmd xs                         = Left  $ UnknownCommand xs

||| Given a root directory for *pack* and a db version,
||| generates the application
||| config from a list of command line arguments.
export
applyArgs :  (dir  : String)
          -> (db   : String)
          -> (args : List String)
          -> Either PackErr Config
applyArgs dir db args =
  case getOpt RequireOrder descs args of
       MkResult opts n  []      []       =>
         let conf = foldl (flip apply) (init dir db) opts
          in map (\p => {cmd := p} conf) (cmd n)

       MkResult _    _ (u :: _) _        => Left (UnknownArg u)
       MkResult _    _ _        (e :: _) => Left (ErroneousArg e)

--------------------------------------------------------------------------------
--          Usage Info
--------------------------------------------------------------------------------

version : String
version = "0.0.1"

progName : String
progName = "pack"

||| Application info printed with the `--help` action.
export
usageInfo : String
usageInfo = """
  Usage: \{progName} [options] COMMAND [args]

  Options:
  \{usageInfo "" descs}

  Commands:
    help
      Print this help text.

    build <.ipkg file>
      Build a local package given as an `.ipkg` file.

    typecheck <.ipkg file>
      Typecheck a local package given as an `.ipkg` file.

    install [package or .ipkg file...]
      Install the given package(s) and/or local .ipkg files.

    install-with-src [package or .ipkg file...]
      Install the given package(s) and/or local .ipkg files
      together with their sources.

    install-app [package or .ipkg file...]
      Install the given application(s).

    remove [package or .ipkg file...]
      Remove installed librarie(s).

    switch <repositoy>
      Change the repository `$PACK_DIR/bin` points
      to. If `$PACK_DIR/bin` is on your path, all applications
      installed for the given repository (including Idris2
      itself) will be on your path as well.
      Note: This will install Idris2 and *pack* form the
      given repository so it may take some time.

    update-db
      Update the pack data base by downloading the package collections
      from https://github.com/stefan-hoeck/idris2-pack-db.

    exec <package or .ipkg file> [args]
      Build and run an executable given either as
      an `.ipkg` file or a known package from the
      database passing it the given command line arguments.

    check-db <repository>
      Check the given package collection by freshly
      building and installing its designated Idris2 executable
      followed by installing all listed packages.
  """
