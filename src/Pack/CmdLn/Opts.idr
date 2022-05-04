module Pack.CmdLn.Opts

import Data.String
import Libraries.Utils.Path
import Pack.CmdLn.Types
import Pack.Core.Types
import System.Console.GetOpt

%default total

dir : String -> Config s -> Config s
dir s = {packDir := parse s}

bootstrap : Config s -> Config s
bootstrap = {bootstrap := True}

setDB : String -> Config s -> Config s
setDB s = {dbVersion := MkDBName s}

setScheme : String -> Config s -> Config s
setScheme s = {scheme := parse s}

-- command line options with description
descs : List $ OptDescr (Config Nothing -> Config Nothing)
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
        , MkOpt [] ["bootstrap"]   (NoArg bootstrap)
            """
            Use bootstrapping when building the Idris2 compiler.
            This is for users who don't have a recent version of
            the Idris2 compiler on their `$PATH`. Compiling Idris2
            will take considerably longer with this option set.
            """
        ]

export
optionNames : List String
optionNames = foldMap names descs
  where names : OptDescr a -> List String
        names (MkOpt sns lns _ _) =
          map (\c => "-\{String.singleton c}") sns ++ map ("--" ++) lns


cmd : List String -> Either PackErr Cmd
cmd []                         = Right PrintHelp
cmd ["help"]                   = Right PrintHelp
cmd ["update-db"]              = Right UpdateDB
cmd ["check-db", db]           = Right $ CheckDB (MkDBName db)
cmd ("exec" :: file :: args)   = Right $ Exec (fromString file) args
cmd ["extract-from-head", p]   = Right $ FromHEAD (parse p)
cmd ["build", file]            = Right $ Build (parse file)
cmd ["typecheck", file]        = Right $ Typecheck (parse file)
cmd ["switch", repo]           = Right $ SwitchRepo (MkDBName repo)
cmd ("install" :: xs)          = Right $ Install (map fromString xs)
cmd ("remove" :: xs)           = Right $ Remove (map fromString xs)
cmd ("install-with-src" :: xs) = Right $ InstallWithSrc (map fromString xs)
cmd ("install-app" :: xs)      = Right $ InstallApp (map fromString xs)
cmd ["completion",a,b]         = Right $ Completion a b
cmd ["completion-script",f]    = Right $ CompletionScript f
cmd xs                         = Left  $ UnknownCommand xs

||| Given a root directory for *pack* and a db version,
||| generates the application
||| config from a list of command line arguments.
export
applyArgs :  (dir  : Path)
          -> (db   : DBName)
          -> (args : List String)
          -> Either PackErr (Config Nothing)
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

    extract-from-head <output file>
      Extracts a new unstable data collection from the HEAD
      colletion by querying the GitHub repository of every
      package for the latest commit and writing everything in
      a new file and stores it in the given file.
  """
