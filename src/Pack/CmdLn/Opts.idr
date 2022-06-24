module Pack.CmdLn.Opts

import Pack.CmdLn.Types
import Pack.Core.Types
import Pack.Config.Types
import System.Console.GetOpt

%default total

-- Function for adjusting the config based on a command
-- line option. This first argument is the current directory
-- from which the application was invoked.
0 AdjConf : Maybe State -> Type
AdjConf s = Path Abs -> Config s -> Either PackErr (Config s)

debug : AdjConf s
debug _ = Right . {logLevel := Debug}

bootstrap : AdjConf s
bootstrap _ = Right . {bootstrap := True}

withSrc : AdjConf s
withSrc _ = Right . {withSrc := True}

setDB : String -> AdjConf s
setDB s _ c = map (\db => {collection := db} c) $ readDBName s

setQuery : QueryType -> AdjConf s
setQuery s _ = Right . {queryType := s}

setPrompt : Bool -> AdjConf s
setPrompt b _ = Right . {safetyPrompt := b}

setScheme : String -> AdjConf s
setScheme s _ = Right . {scheme := fromString s}

setRlwrap : Bool -> AdjConf s
setRlwrap b _ = Right . {rlwrap := b }

setIpkg : String -> AdjConf s
setIpkg v dir c = case readAbsFile dir v of
  Right af => Right $ {withIpkg := Use af} c
  Left err => Left err

noIpkg : AdjConf s
noIpkg _ = Right . {withIpkg := None}

codegen : String -> AdjConf s
codegen v _ = Right . {codegen := fromString v}

-- command line options with description
descs : List $ OptDescr (AdjConf Nothing)
descs = [ MkOpt ['p'] ["package-set"]   (ReqArg setDB "<db>")
            "Set the curated package set to use."
        , MkOpt [] ["cg"]   (ReqArg codegen "<codgen>")
            """
            Sets the backend to use when building Idris executables
            or running the REPL. The default is to use the `chez`
            code generator.
            """
        , MkOpt [] ["scheme"]   (ReqArg setScheme "<exec>")
            """
            Sets the scheme executable for installing the Idris2 compiler.
            As a default, this is set to `scheme`.
            """
        , MkOpt ['s'] ["short-desc"]   (NoArg $ setQuery ShortDesc)
            """
            Print the short description stored in an `.ipkg` file for
            each query result.
            """
        , MkOpt ['l'] ["long-desc"]   (NoArg $ setQuery Details)
            "Print a detailed description of a package known to pack"
        , MkOpt ['d'] ["dependencies"]   (NoArg $ setQuery Dependencies)
            "Print the dependencies of each query result."
        , MkOpt [] ["ipkg"]   (NoArg $ setQuery Ipkg)
            "Print the full `.ipkg` file of each query result."
        , MkOpt [] ["bootstrap"]   (NoArg bootstrap)
            """
            Use bootstrapping when building the Idris2 compiler.
            This is for users who don't have a recent version of
            the Idris2 compiler on their `$PATH`. Compiling Idris2
            will take considerably longer with this option set.
            """
        , MkOpt [] ["prompt"]   (NoArg $ setPrompt True)
            """
            Prompt before installing a potentially unsafe package
            with custom build hooks.
            """
        , MkOpt [] ["no-prompt"]   (NoArg $ setPrompt False)
            """
            Don't prompt before installing a potentially unsafe package
            with custom build hooks.
            """
        , MkOpt [] ["with-src"]   (NoArg withSrc)
            """
            Include the source code of a library when installing
            it. This allows some editor plugins to jump to the
            definitions of functions and data types in other
            modules.
            """
        , MkOpt [] ["with-ipkg"]   (ReqArg setIpkg "<.ipkg>")
            """
            Use settings and packages from the given `.ipkg` file when
            starting a REPL session.
            """
        , MkOpt [] ["no-ipkg"]   (NoArg noIpkg)
            """
            Don't look for an `.ipkg` file in scope when starting a REPL session.
            """
        , MkOpt [] ["rlwrap"]   (NoArg $ setRlwrap True)
            "Run a REPL session in `rlwrap`."
        , MkOpt ['v'] ["verbose"]   (NoArg debug)
            "Print debugging information"
        ]

export
optionNames : List String
optionNames = foldMap names descs
  where names : OptDescr a -> List String
        names (MkOpt sns lns _ _) =
          map (\c => "-\{String.singleton c}") sns ++ map ("--" ++) lns

ipkgFile : Path Abs -> String -> (AbsFile -> Cmd) -> Either PackErr Cmd
ipkgFile dir s f = f <$> readAbsFile dir s

export
cmd : Path Abs -> List String -> Either PackErr Cmd
cmd _   []                         = Right PrintHelp
cmd _   ["help"]                   = Right PrintHelp
cmd _   ["info"]                   = Right Info
cmd _   ["update-db"]              = Right UpdateDB
cmd _   ["fuzzy", s]               = Right $ Fuzzy [] s
cmd _   ["fuzzy", p, s]            = Right $ Fuzzy (forget $ map MkPkgName $ split (',' ==) p) s
cmd _   ["query", s]               = Right $ Query PkgName s
cmd _   ["query", "dep", s]        = Right $ Query Dependency s
cmd _   ["query", "module", s]     = Right $ Query Module s
cmd _   ["repl"]                   = Right $ Repl Nothing
cmd dir ["repl", s]                = Repl . Just <$> readAbsFile dir s
cmd dir ("run" :: p :: args)       =
  let deflt   := Right $ Run (Right $ MkPkgName p) args
      Right af := readAbsFile dir p | Left _ => deflt
   in case isIpkgBody af.file of
     True  => Right $ Run (Left af) args
     False => deflt
cmd dir ["build", file]            = ipkgFile dir file Build
cmd dir ["install-deps", file]     = ipkgFile dir file BuildDeps
cmd dir ["typecheck", file]        = ipkgFile dir file Typecheck
cmd _   ("install" :: xs)          = Right $ Install (map fromString xs)
cmd _   ("remove" :: xs)           = Right $ Remove (map fromString xs)
cmd _   ("install-app" :: xs)      = Right $ InstallApp (map fromString xs)
cmd _   ["completion",a,b]         = Right $ Completion a b
cmd _   ["completion-script",f]    = Right $ CompletionScript f
cmd _   ["package-path"]           = Right PackagePath
cmd _   ["libs-path"]              = Right LibsPath
cmd _   ["data-path"]              = Right DataPath
cmd _   ["switch",db]              = Switch <$> readDBName db
cmd _  xs                          = Left  $ UnknownCommand xs

||| Given a root directory for *pack* and a db version,
||| generates the application
||| config from a list of command line arguments.
export
applyArgs :  (curDir  : Path Abs)
          -> (init    : Config Nothing)
          -> (args    : List String)
          -> (readCmd : List String -> Either PackErr a)
          -> Either PackErr (Config Nothing, a)
applyArgs dir init args readCmd =
  case getOpt RequireOrder descs args of
       MkResult opts n  []      []       =>
         let conf = foldlM (\c,f => f dir c) init opts
          in [| MkPair conf (readCmd n) |]

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

    install-deps <.ipkg file>
      Install the dependencies of a local package given as an `.ipkg` file.

    typecheck <.ipkg file>
      Typecheck a local package given as an `.ipkg` file.

    repl [.idr file]
      Start a REPL session loading an optional `.idr` file.
      Use command line option `--with-ipkg` to load settings
      and packages from an `.ipkg` file. Option `--no-ipkg` can be used
      to not go looking for an `.ipkg` file. The default behavior
      is for pack to use the first `.ipkg` file it can find in the
      current directory or one of its prent directories.

      In order to start the REPL session with `rlwrap`, use the `--rlwrap`
      option or set flag `repl.rlwrap` in file
      `$HOME./pack/user/pack.toml` to `true`.

    install [package...]
      Install the given packages.

    install-app [package...]
      Install the given applications.

    run <package or .ipkg file> [args]
      Run an application from the package
      collection or a local `.ipkg` file passing it the
      given command line arguments.

      Note: This will install remote apps before running them.
      Local apps and applications specified as mere `.ipkg` files
      will be built and run locally without installing them.

    remove [package...]
      Remove installed libraries and applications.

    update-db
      Update the pack data base by downloading the package collections
      from https://github.com/stefan-hoeck/idris2-pack-db.

    switch [collection name]
      Switch to the given package collection. This will make all
      binaries installed for this collection available in folder
      `$HOME/.pack/bin`, which you can then include in your
      `$PATH` variable.

      Note: It is also possible to switch to the latest package
      collection by using "latest" as the collection name. This will
      automatically update the data collection, select the latest
      nightly, and change the `collection =` entry in the global
      `pack.toml` file.

    info
      Print general information about the current package
      collection and list installed applications and libraries.

    query [mode] <substring>
      Query the package collection for the given name.
      Several command line options exist to define the type
      of information printed. The optional [mode] argument
      defines the type of query to use:

        * `dep`    : Search a package by its dependencies. For
          instance, `pack query dep sop` will list all packages,
          which have a dependency on the sop library. Only exact
          matches will be listed.

        * `module` : Search a package by its modules. For
          instance, `pack query module Data.List` will list all packages,
          which export module `Data.List`. Only exact matches will
          be listed.

        * none     : List packages whose names have the query
          string as a substring.

    fuzzy [packages] <query>
      Run a fuzzy search by type over a comma-separated list of packages.
      If no packages are given, all installed packages will be queried
      (which might take several minutes).

      Examples: fuzzy base "HasIO -> Bool", will find functions taking
      an argument of type `HasIO` and returning a boolean result.


    package-path
      Return a colon-separated list of paths where packages are
      installed. This is useful for programs like `idris2-lsp`,
      which need to know where to look for installed packages.

    libs-path
      Return a colon-separated list of paths where libraries
      for code generation are installed.

    data-path
      Return a colon-separated list of paths where data files
      are installed.
  """
