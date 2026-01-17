module Pack.CmdLn.Types

import Data.List.Elem
import Pack.Core
import Pack.Config.Types
import Pack.Database.Types

%default total

||| Mode used for querying the package collection:
||| By package name, by dependency, or by module name.
public export
data QueryMode = PkgName | Dependency | Module

||| A query to be run against the package collection
public export
record PkgQuery where
  constructor MkQ
  mode  : QueryMode
  query : String

export
Arg PkgQuery where
  argDesc_ = "[mode] <query>"

  readArg ("dep" :: s :: t)    = Just (MkQ Dependency s, t)
  readArg ("module" :: s :: t) = Just (MkQ Module s, t)
  readArg [s]                  = Just (MkQ PkgName s, [])
  readArg _                    = Nothing

public export
record FuzzyQuery where
  constructor MkFQ
  pkgs  : List PkgName
  query : String

export
Arg FuzzyQuery where
  argDesc_ = "[<pkgs>] <query>"

  readArg (ps :: s :: t) =
    let pkgs = map MkPkgName $ forget $ split (',' ==) ps
     in Just (MkFQ pkgs s, t)
  readArg [s] = Just (MkFQ [] s, [])
  readArg []  = Nothing

||| Trivial Commands accepted by *pack*. These commands do not require
||| information about pack's configuration on the system.
public export
data TrivialCmd : Type where
  -- Tab completion
  CompletionScript : TrivialCmd

||| Configured Commands accepted by *pack*. These commands require information
||| about pack's configuration on the system.
public export
data ConfiguredCmd : Type where
  -- Developing Idris libs and apps
  Build            : ConfiguredCmd
  BuildDeps        : ConfiguredCmd
  Typecheck        : ConfiguredCmd
  Clean            : ConfiguredCmd
  CleanBuild       : ConfiguredCmd
  Repl             : ConfiguredCmd
  Exec             : ConfiguredCmd

  -- Package management
  Install          : ConfiguredCmd
  InstallApp       : ConfiguredCmd
  Remove           : ConfiguredCmd
  RemoveApp        : ConfiguredCmd
  Run              : ConfiguredCmd
  Test             : ConfiguredCmd
  New              : ConfiguredCmd
  Update           : ConfiguredCmd
  Fetch            : ConfiguredCmd

  -- Idris environment
  PackagePath      : ConfiguredCmd
  LibsPath         : ConfiguredCmd
  DataPath         : ConfiguredCmd
  AppPath          : ConfiguredCmd

  -- Managing package collections
  Switch           : ConfiguredCmd
  UpdateDB         : ConfiguredCmd
  CollectGarbage   : ConfiguredCmd

  -- Queries
  Info             : ConfiguredCmd
  Query            : ConfiguredCmd
  Fuzzy            : ConfiguredCmd

  -- Tab completion
  Completion       : ConfiguredCmd
  -- CompletionScript : Cmd (this is a trivial command)

  -- Uninstall
  Uninstall        : ConfiguredCmd

  -- Help
  PrintHelp        : ConfiguredCmd

||| Commands accepted by *pack*. Most of these
||| operate on a list of packages and/or
||| projects with an `.ipkg` file.
public export
data Cmd = Trivial TrivialCmd
         | Configured ConfiguredCmd

||| Name to use at the command-line for running a pack command
public export
name : Cmd -> String

namespace Configured
  ||| List of all available configured commands.
  |||
  ||| `Pack.CmdLn.Types.cmdInCommands` proves that none were forgotten.
  public export
  commands : List ConfiguredCmd
  commands =
    [ Build
    , BuildDeps
    , Typecheck
    , Clean
    , CleanBuild
    , Repl
    , Exec
    , Install
    , InstallApp
    , Remove
    , RemoveApp
    , Run
    , Test
    , New
    , Update
    , Fetch
    , PackagePath
    , LibsPath
    , DataPath
    , AppPath
    , Switch
    , UpdateDB
    , CollectGarbage
    , Info
    , Query
    , Fuzzy
    , Completion
    , Uninstall
    , PrintHelp
    ]

  ||| Name to use at the command-line for running a pack command
  public export
  name : ConfiguredCmd -> String
  name (Build           ) = "build"
  name (BuildDeps       ) = "install-deps"
  name (Typecheck       ) = "typecheck"
  name (Clean           ) = "clean"
  name (CleanBuild      ) = "cleanbuild"
  name (Repl            ) = "repl"
  name (Exec            ) = "exec"
  name (Install         ) = "install"
  name (InstallApp      ) = "install-app"
  name (Remove          ) = "remove"
  name (RemoveApp       ) = "remove-app"
  name (Run             ) = "run"
  name (Test            ) = "test"
  name (New             ) = "new"
  name (Update          ) = "update"
  name (Fetch           ) = "fetch"
  name (PackagePath     ) = "package-path"
  name (LibsPath        ) = "libs-path"
  name (DataPath        ) = "data-path"
  name (AppPath         ) = "app-path"
  name (Switch          ) = "switch"
  name (UpdateDB        ) = "update-db"
  name (CollectGarbage  ) = "gc"
  name (Info            ) = "info"
  name (Query           ) = "query"
  name (Fuzzy           ) = "fuzzy"
  name (Completion      ) = "completion"
  name (Uninstall       ) = "uninstall"
  name (PrintHelp       ) = "help"

  ||| List pairing a command with its name used for parsing commands.
  public export
  namesAndCommands : List (String, ConfiguredCmd)
  namesAndCommands = map (\c => (name c, c)) commands

  ||| Usage info for each configured command. This is printed when invoking `pack help <cmd>`.
  export
  cmdDesc : ConfiguredCmd -> String
  cmdDesc (Build)      = """
    Build a local package given as an `.ipkg` file or package name.
    When no package is given, try to find the only one in the current directory.
    This will also install the package's dependencies.
    """

  cmdDesc (BuildDeps)  = """
    Install the dependencies of a local package given as an `.ipkg` file
    or package name. When no package is given, try to find the only one
    in the current directory.
    """

  cmdDesc (Typecheck)  = """
    Typecheck a local package given as an `.ipkg` file or package name.
    When no package is given, try to find the only one in the current directory.
    """

  cmdDesc (Clean)      = """
    Clean up a local package by removing its build directory.
    When no package is given, try to find the only one in the current directory.
    """

  cmdDesc (CleanBuild) = """
    Convenience combination of `clean` followed by `build`.
    """

  cmdDesc (Repl)       = """
    Start a REPL session loading an optional `.idr` file.
    Use command line option `--with-ipkg` to load settings
    and packages from an `.ipkg` file. Option `--no-ipkg` can be used
    to not go looking for an `.ipkg` file. The default behavior
    is for pack to use the first `.ipkg` file it can find in the
    current directory or one of its parent directories.

    In order to start the REPL session with the `rlwrap` utility, use
    the `--rlwrap` command-line option or set flag `repl.rlwrap` in one
    of your `pack.toml` files either to `true` or to a string containing
    additional arguments to be used when running `rlwrap`.
    """

  cmdDesc (Exec)       = """
    Compile the given Idris source file and execute its main function
    with the given list of arguments. This will look for `.ipkg` files
    in the source file's parent directories and will apply the settings
    it finds there.

    To change the name of the generated executable, use the `-o` command-line
    option.

    To change the codegen to use, use the `--cg` command-line option.
    """

  cmdDesc (Install)    = "Install the given packages."

  cmdDesc (InstallApp) = "Install the given applications."

  cmdDesc (Remove)     = "Uninstall the given libraries."

  cmdDesc (RemoveApp)  = "Uninstall the given applications."

  cmdDesc (Run)        = """
    Run an application from the package collection or a local `.ipkg`
    file passing it the given command line arguments.
    When no package and no arguments are given, try to find the only one
    in the current directory.

    Note: This will install remote apps before running them without
    generating an entry in `$PACK_DIR/bin`.
    Local apps and applications specified as mere `.ipkg` files
    will be built and run locally without installing them.

    To change the codegen to use, use the `--cg` command-line option.
    """

  cmdDesc (Test)       = """
    Run a test suite as specified in a package description's `test` field.

    The `test` field should consist of a file path relative to a package's
    root directory point to the test suite's `.ipkg` file:

    [custom.all.json]
    type   = "local"
    path   = "."
    ipkg   = "json.ipkg"
    test   = "test/test.ipkg"
    """

  cmdDesc (New)        = """
    Create a new package in the current directory
    consisting of a source directory, a default module, a skeleton test suite, a local pack.toml file and a .ipkg file.

    Passing the `--git-init` command line option will
    create a .gitignore file and a .git directory.

    Note: Since module names with a hyphen ('-') are not supported by
    Idris, any hyphen in the package name will be replaced with an
    underscore ('_') in the generated module name.
    """

  cmdDesc (Update)     = """
    Update the pack installation by downloading and building
    the current main branch of
    https://github.com/stefan-hoeck/idris2-pack.

    In order to specify a different commit or repository to use, adjust
    settings `pack.url` and `pack.commit` in one of your `pack.toml`
    files.

    Note: This uses the current package collection, which might be
    too outdated to build the latest pack. If this fails, try using
    the latest nightly.
    """

  cmdDesc (Fetch)      = """
    Fetch the latest commit hashes from a repository for Git packages with a
    commit entry of "latest:branch".
    """

  cmdDesc (PackagePath) = """
    Return a colon-separated list of paths where Idris packages are
    installed. This is useful for programs like `idris2-lsp`,
    which need to know where to look for installed packages.
    """

  cmdDesc (LibsPath)   = """
    Return a colon-separated list of paths where libraries
    for code generation are installed.
    """

  cmdDesc (DataPath)   = """
    Return a colon-separated list of paths where data files
    are installed.
    """

  cmdDesc (AppPath)    = """
    Return the absolute path to the given application managed by pack.
    `pack app-path idris2` returns the path to the current Idris compiler
    """

  cmdDesc (Switch)     = """
    Switch to the given package collection. This will adjust your
    `$PACK_DIR/user/pack.toml` file to use the given package
    collection. It will also install all auto libs and apps from the
    given package collection.

    Note: It is also possible to switch to the latest package
    collection by using "latest" as the collection name.
    """

  cmdDesc (UpdateDB)   = """
    Update the pack data base by downloading the package collections
    from https://github.com/stefan-hoeck/idris2-pack-db.
    """

  cmdDesc (CollectGarbage) = """
    Clean up installations of older package collections by removing
    all sub-directories of `$PACK_STATE_DIR/install` not belonging to the
    currently used compiler commit.

    In case the `--gc-purge` option is set, this will also remove all
    outdated libraries installed with the current compiler commit.
    """

  cmdDesc (Info)       = """
    Print general information about the current package
    collection and list installed applications and libraries.
    """

  cmdDesc (Query)      = """
    Query the package collection for the given name.
    Several command line options exist to specify the type
    of information printed. The optional mode argument
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

    The following command-line options affect the kind of information displayed
    for each found package:

      -d --dependencies : Prints the dependencies of each query result
         --tree         : Prints a dependency tree of a queried package
         --reverse-tree : Prints a tree of packages depending on a queried package
      -s --short-desc   : Prints the `brief` description from each package's
                          `.ipkg` file
      -l --long-desc    : Prints detailed description of each query result
    """

  cmdDesc (Fuzzy)      = """
    Run a fuzzy search by type over a comma-separated list of packages.
    If no packages are given, all installed packages will be queried
    (which might take several minutes).

    Example: fuzzy base "HasIO -> Bool", will find functions taking
    an argument of type `HasIO` and returning a boolean result in the
    *base* library.
    """

  cmdDesc (Completion) = """
    Returns a list of possible completion strings for the given arguments.
    This is invoked by the shell script returned by `pack \{name $ Trivial CompletionScript}`.
    See the installation instructions about how to enable TAB-completion
    for your shell.
    """

  cmdDesc (Uninstall)  = """
    Uninstalls pack.
    Deletes the $PACK_DIR directory.
    """

  cmdDesc (PrintHelp)  = """
    Without an additional <cmd> argument, this prints general information
    about using pack, including a list of available command-line options
    and a description of what each of them does.

    If an explicit command is given, this gives some detail about what
    the command in question does and what additional arguments it takes.

    Available commands:
    \{unlines $ map (indent 2 . fst) namesAndCommands}
    """

namespace Trivial
  ||| List of all available trivial commands.
  |||
  ||| `Pack.CmdLn.Types.cmdInCommands` proves that none were forgotten.
  public export
  commands : List TrivialCmd
  commands =
    [ CompletionScript
    ]

  ||| Name to use at the command-line for running a pack command
  public export
  name : TrivialCmd -> String
  name (CompletionScript) = "completion-script"

  ||| List pairing a command with its name used for parsing commands.
  public export
  namesAndCommands : List (String, TrivialCmd)
  namesAndCommands = map (\c => (name c, c)) commands

  ||| Usage info for each trivial command. This is printed when invoking `pack help <cmd>`.
  export
  cmdDesc : TrivialCmd -> String
  cmdDesc (CompletionScript) = """
    Prints a shell script, which can be used for BASH-like TAB-completion.
    See the installation instructions about how to enable TAB-completion
    for your shell.
    """

-- Declared above --
--
-- public export
-- name : Cmd -> String
name (Trivial cmd) = Trivial.name cmd
name (Configured cmd) = Configured.name cmd

||| List pairing a command with its name used for parsing commands.
public export
namesAndCommands : List (String, Cmd)
namesAndCommands =
  map (\c => (name c, Configured c)) Configured.commands
  ++ map (\c => (name c, Trivial c)) Trivial.commands

export
Arg ConfiguredCmd where
  argDesc_ = "<cmd>"

  readArg = parseSingleMaybe (`lookup` namesAndCommands)

export
Arg TrivialCmd where
  argDesc_ = "<cmd>"

  readArg = parseSingleMaybe (`lookup` namesAndCommands)

export
Arg Cmd where
  argDesc_ = "<cmd>"

  readArg = parseSingleMaybe (`lookup` namesAndCommands)

--------------------------------------------------------------------------------
--          Proofs
--------------------------------------------------------------------------------

0 configuredCmdInCommands : (c : ConfiguredCmd) -> Elem c Configured.commands
configuredCmdInCommands (Build           ) = %search
configuredCmdInCommands (BuildDeps       ) = %search
configuredCmdInCommands (Typecheck       ) = %search
configuredCmdInCommands (Clean           ) = %search
configuredCmdInCommands (CleanBuild      ) = %search
configuredCmdInCommands (Repl            ) = %search
configuredCmdInCommands (Exec            ) = %search
configuredCmdInCommands (Install         ) = %search
configuredCmdInCommands (InstallApp      ) = %search
configuredCmdInCommands (Remove          ) = %search
configuredCmdInCommands (RemoveApp       ) = %search
configuredCmdInCommands (Run             ) = %search
configuredCmdInCommands (Test            ) = %search
configuredCmdInCommands (New             ) = %search
configuredCmdInCommands (Update          ) = %search
configuredCmdInCommands (Fetch           ) = %search
configuredCmdInCommands (PackagePath     ) = %search
configuredCmdInCommands (LibsPath        ) = %search
configuredCmdInCommands (DataPath        ) = %search
configuredCmdInCommands (AppPath         ) = %search
configuredCmdInCommands (Switch          ) = %search
configuredCmdInCommands (UpdateDB        ) = %search
configuredCmdInCommands (CollectGarbage  ) = %search
configuredCmdInCommands (Info            ) = %search
configuredCmdInCommands (Query           ) = %search
configuredCmdInCommands (Fuzzy           ) = %search
configuredCmdInCommands (Completion      ) = %search
configuredCmdInCommands (Uninstall       ) = %search
configuredCmdInCommands (PrintHelp       ) = %search

0 trivialCmdInCommands : (c : TrivialCmd) -> Elem c Trivial.commands
trivialCmdInCommands (CompletionScript) = %search
