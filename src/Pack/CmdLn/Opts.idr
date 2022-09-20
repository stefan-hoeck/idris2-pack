module Pack.CmdLn.Opts

import Pack.CmdLn.Types
import Pack.Core
import Pack.Database
import Pack.Config.Types
import System.Console.GetOpt

%default total

-- Function for adjusting the config based on a command
-- line option. This first argument is the current directory
-- from which the application was invoked.
0 AdjConf : Type
AdjConf = CurDir -> MetaConfig -> Either PackErr MetaConfig

debug : AdjConf
debug _ = Right . {logLevel := Debug}

withSrc : AdjConf
withSrc _ = Right . {withSrc := True}

withDocs : AdjConf
withDocs _ = Right . {withDocs := True}

useKatla : AdjConf
useKatla _ = Right . {useKatla := True}

setDB : String -> AdjConf
setDB s _ c = map (\db => {collection := db} c) $ readDBName s

setOutput : String -> AdjConf
setOutput s _ c = map (\o => {output := o} c) $ readBody s

setQuery : QueryType -> AdjConf
setQuery s _ = Right . {queryType := s}

setPrompt : Bool -> AdjConf
setPrompt b _ = Right . {safetyPrompt := b}

setScheme : String -> AdjConf
setScheme s _ = Right . {scheme := fromString s}

setRlwrap : Bool -> AdjConf
setRlwrap b _ = Right . {rlwrap := b }

setIpkg : String -> AdjConf
setIpkg v (CD dir) c = case readAbsFile dir v of
  Right af => Right $ {withIpkg := Use af} c
  Left err => Left err

setPkgs : String -> AdjConf
setPkgs str _ c =
  let ps = map MkPkgName . forget $ split (',' ==) str
   in Right $ {autoLoad := ForcePkgs ps} c

noIpkg : AdjConf
noIpkg _ = Right . {withIpkg := None}

codegen : String -> AdjConf
codegen v _ = Right . {codegen := fromString v}

-- command line options with description
descs : List $ OptDescr AdjConf
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
        , MkOpt ['P'] ["packages"]  (ReqArg setPkgs "<packages>")
            """
            Load the given (comma-separated) list of packages into the REPL
            session.
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
        , MkOpt ['o'] ["output"]   (ReqArg  setOutput "<file>")
            """
            Name of the output file when compiling or running single Idris source files
            This defaults to `_tmppack` if not specified explicitly"
            """
        , MkOpt [] ["ipkg"]   (NoArg $ setQuery Ipkg)
            "Print the full `.ipkg` file of each query result."
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
        , MkOpt [] ["with-docs"]   (NoArg withDocs)
            """
            Include the API documentation when installing libraries.
            """
        , MkOpt [] ["use-katla"]   (NoArg useKatla)
            """
            Use katla to add links to the semantically highlighted API sources.
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

||| Names of all command line options (prefixed with "-" in case of
||| single-character option names and with "--" in case of multi-character
||| option names.
export
optionNames : List String
optionNames = foldMap names descs
  where names : OptDescr a -> List String
        names (MkOpt sns lns _ _) =
          map (\c => "-\{String.singleton c}") sns ++ map ("--" ++) lns

||| Given the current directory (from which pack was invoked)
||| and an initial config assembled from the `pack.toml` files
||| in scope, generates the application
||| config and command to run from a list of command
||| line arguments.
|||
||| @ c      : Type representing the command to run
|||            We abstract over this type, because pack and
|||            pack-admin accept different kinds of commands,
|||            and both applications use this function to parse
|||            the command line args
|||
||| @ curDir : Current working directory
||| @ init   : Initial config (possibly assebled from `pack.toml` files)
||| @ args   : List of command line arguments
export
applyArgs :  (0 c : Type)
          -> Command c
          => (curDir     : CurDir)
          -> (init       : MetaConfig)
          -> (args       : List String)
          -> Either PackErr (MetaConfig, c)
applyArgs c dir init args =
  case getOpt RequireOrder descs args of
       MkResult opts n  []      []       => do
         cmd  <- readCommand c dir n
         let init' = {logLevel := defaultLevel cmd} init
         conf <- foldlM (\c,f => f dir c) init' opts
         Right (conf, cmd)

       MkResult _    _ (u :: _) _        => Left (UnknownArg u)
       MkResult _    _ _        (e :: _) => Left (ErroneousArg e)

--------------------------------------------------------------------------------
--          Usage Info
--------------------------------------------------------------------------------

progName : String
progName = "pack"

||| Application info printed with the `help` action.
export
usageInfo : String
usageInfo = """
  Usage: \{progName} [options] COMMAND [args]

  Options:
  \{usageInfo "" descs}

  Commands:
    help
      Print this help text.

    new <lib | bin> <package name>
      Create a new package in the current directory
      consisting of a source directory, default module and a .ipkg file.
      A git repository will be initialized.

    build <.ipkg file or local pkg name>
      Build a local package given as an `.ipkg` file or package name.

    exec <.idr file> [args...]
      Compile the given Idris source file and execute its main function
      with the given list of arguments. This will look for `.ipkg` files
      in the source file's parent directories and will apply the settings
      it finds there.

    install-deps <.ipkg file or local pkg name>
      Install the dependencies of a local package given as an `.ipkg` file
      or package name.

    typecheck <.ipkg file or local pkg name>
      Typecheck a local package given as an `.ipkg` file or package name.

    repl [.idr file]
      Start a REPL session loading an optional `.idr` file.
      Use command line option `--with-ipkg` to load settings
      and packages from an `.ipkg` file. Option `--no-ipkg` can be used
      to not go looking for an `.ipkg` file. The default behavior
      is for pack to use the first `.ipkg` file it can find in the
      current directory or one of its prent directories.

      In order to start the REPL session with `rlwrap`, use the `--rlwrap`
      option or set flag `repl.rlwrap` in file
      `$HOME/.pack/user/pack.toml` to `true`.

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
      Remove installed libraries.

    remove-app [package...]
      Remove installed applications.

    update-db
      Update the pack data base by downloading the package collections
      from https://github.com/stefan-hoeck/idris2-pack-db.

    update
      Update the pack installation by downloading and building
      the current main branch of
      https://github.com/stefan-hoeck/idris2-pack.

      Note: This uses the current package collection, which might be
      too outdated to build the latest pack. If this fails, try using
      the latest nightly.

    fetch
      Fetch latest commit hashes from GitHub for packages with a
      commit entry of "latest:branch".

    switch [collection name]
      Switch to the given package collection. This will adjust your
      `$HOME/.pack/user/pack.toml` file to use the given package
      collection. It will also install all auto libs and apps for the
      given package collection.

      Note: It is also possible to switch to the latest package
      collection by using "latest" as the collection name.

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

    app-path <pkgname>
      Return the absolute path to the given application managed by pack.
      `pack app-path idris2` returns the path to the current Idris compiler
  """
