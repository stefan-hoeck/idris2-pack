module Pack.CmdLn.Opts

import Data.SortedMap
import Pack.CmdLn.Types
import Pack.Core
import Pack.Database
import Pack.Config.Types
import System.GetOpts

%default total

-- Function for adjusting the config based on a command
-- line option. This first argument is the current directory
-- from which the application was invoked.
0 AdjConf : Type
AdjConf = CurDir -> MetaConfig -> Either PackErr MetaConfig

debug : AdjConf
debug _ = Right . {logLevel := Debug}

quiet : AdjConf
quiet _ = Right . {logLevel := Silence}

loglevel : String -> AdjConf
loglevel str _ c = map (\ll => {logLevel := ll} c) (readLogLevel str)

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

setGCPrompt : Bool -> AdjConf
setGCPrompt b _ = Right . {gcPrompt := b}

setWarnDepends : Bool -> AdjConf
setWarnDepends b _ = Right . {warnDepends := b}

setSkipTests : Bool -> AdjConf
setSkipTests b _ = Right . {skipTests := b}

setScheme : String -> AdjConf
setScheme s _ = Right . {scheme := fromString s}

setBootstrap : Bool -> AdjConf
setBootstrap b _ = Right . {bootstrap := b}

setRlwrap : Maybe String -> AdjConf
setRlwrap args _ = Right . {rlwrap := UseRlwrap $ maybe [] (\s => [NoEscape s]) args}

addExtraArgs : String -> AdjConf
addExtraArgs args _ = Right . {extraArgs $= (++ [NoEscape args])}

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
descs =
  [ MkOpt ['p'] ["package-set"]   (ReqArg setDB "<db>")
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
  , MkOpt [] ["tree"]   (NoArg $ setQuery Tree)
      "Print a dependency tree of a package known to pack"
  , MkOpt [] ["reverse-tree"]   (NoArg $ setQuery ReverseTree)
      """
      Print a tree of packages depending on a package know to pack.
      Use this to find all packages transitively depending on a specific
      library
      """
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
  , MkOpt [] ["gc-prompt"]   (NoArg $ setGCPrompt True)
      """
      Prompt before deleting directories when running command `gc`.
      """
  , MkOpt [] ["no-gc-prompt"]   (NoArg $ setGCPrompt False)
      """
      Don't prompt before deleting directories when running command `gc`.
      """
  , MkOpt [] ["bootstrap"]   (NoArg $ setBootstrap True)
      """
      Use the bootstrap compiler for building Idris2. This takes longer
      than without bootstrapping, but it will even work if no Idris2
      compiler or an outdated one is on the `$PATH`.
      """
  , MkOpt [] ["no-bootstrap"]   (NoArg $ setBootstrap False)
      """
      Use an existing version of Idris2 when building the compiler.
      This will fail if `idris2` is not on the computer's `$PATH` or
      is too old to build the current version of the compiler.
      """
  , MkOpt [] ["warn-depends"]   (NoArg $ setWarnDepends True)
      """
      Issue a warning in precense of a local `depends` directory, which might
      interfere with the libraries managed by pack.
      """
  , MkOpt [] ["no-warn-depends"]   (NoArg $ setWarnDepends False)
      """
      Don't issue a warning in precense of a local `depends` directory.
      """
  , MkOpt [] ["skip-tests"]   (NoArg $ setSkipTests True)
      """
      Don't run library tests during collection checking.
      This currently only affects the pack-admin utility.
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
  , MkOpt [] ["rlwrap"]   (OptArg setRlwrap "<rlwrap args>")
      "Run a REPL session in `rlwrap`."
  , MkOpt [] ["extra-args"] (ReqArg addExtraArgs "<idris2 args>")
      "Any extra arguments to pass to Idris2."
  , MkOpt ['v'] ["verbose"]   (NoArg debug)
      "Print debugging information"
  , MkOpt ['q'] ["quiet"]   (NoArg quiet)
      "Quiet mode"
  , MkOpt [] ["log-level"]   (ReqArg loglevel "<log level>")
      """
      Specify the logging level to use. Accepted values are:
      \{joinBy ", " $ show . fst <$> logLevels}.
      """
  ]

||| Names of all command line options (prefixed with "-" in case of
||| single-character option names and with "--" in case of multi-character
||| option names.
export
optionNames : List String
optionNames = foldMap names descs

  where
    names : OptDescr a -> List String
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
applyArgs :
     (0 c : Type)
  -> {auto _ : Command c}
  -> (curDir : CurDir)
  -> (init   : MetaConfig)
  -> (args   : List String)
  -> Either PackErr (MetaConfig, CommandWithArgs c)
applyArgs c dir init args =
  case getOpt RequireOrder descs args of
    MkResult opts n  []      []       => do
      cmd  <- readCommand c dir n
      let lvl_m := lookup (cmdName $ fst cmd) init.levels
          dflt  := defaultLevel $ fst cmd

          init' = {logLevel := fromMaybe dflt lvl_m} init
      conf <- foldlM (\c,f => f dir c) init' opts
      Right (conf, cmd)

    MkResult _    _ (u :: _) _        => Left (UnknownArg u)
    MkResult _    _ _        (e :: _) => Left (ErroneousArg e)

--------------------------------------------------------------------------------
--          Usage Info
--------------------------------------------------------------------------------

||| Application info printed with the `help` action.
export
usageInfo : String
usageInfo = """
  Options:
  \{usageInfo "" descs}

  Run `pack help <cmd>` to get detailed information about a command.

  Available commands:
  \{unlines $ map (indent 2 . fst) namesAndCommands}
  """
