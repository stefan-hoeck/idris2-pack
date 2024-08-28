module Pack.Config.Types

import Control.Monad.Either
import Data.Either
import Data.List
import Data.IORef
import public Data.List.Quantifiers
import Data.Maybe
import Data.SortedMap as SM
import Idris.Package.Types
import Libraries.Data.List.Extra
import Pack.Core.Git.Consts
import Pack.Core.Types
import Pack.Database.Types

%default total

||| Packages to automatically load in a REPL session
||| in absence of an `.ipkg` file in scope.
public export
data Autoload : Type where
  ||| Don't load any packages automatically (the default)
  NoPkgs    : Autoload

  ||| Load all installed packages of the current scope
  Installed : Autoload

  ||| Use the packages from the list of packages to be installed
  ||| automatically
  AutoLibs  : Autoload

  ||| Use the given explicit list of packages
  AutoPkgs  : List PkgName -> Autoload

  ||| Use the given explicit list of packages even in the
  ||| precense of an `.ipkg` file.
  ForcePkgs : List PkgName -> Autoload

||| What to show when querying the data base
public export
data QueryType : Type where
  ||| Display only the matching packages' names
  NameOnly     : QueryType

  ||| Display names and short descriptions
  ShortDesc    : QueryType

  ||| List direct dependencies
  Dependencies : QueryType

  ||| Print the full `.ipkg` file
  Ipkg         : QueryType

  ||| List detailed information about a package
  Details      : QueryType

  ||| Print dependency tree
  Tree         : QueryType

  ||| Print inverse dependency tree
  ||| (That is, a tree of packages depending on a given package)
  ReverseTree  : QueryType

||| Code generator to use
public export
data Codegen : Type where
  Default    : Codegen
  Chez       : Codegen
  ChezSep    : Codegen
  Racket     : Codegen
  Gambit     : Codegen
  Node       : Codegen
  JavaScript : Codegen
  RefC       : Codegen
  VMCode     : Codegen
  Other      : String -> Codegen

export
Interpolation Codegen where
  interpolate Default    = ""
  interpolate Chez       = "chez"
  interpolate ChezSep    = "chez-sep"
  interpolate Racket     = "racket"
  interpolate Gambit     = "gambit"
  interpolate Node       = "node"
  interpolate JavaScript = "javascript"
  interpolate RefC       = "refc"
  interpolate VMCode     = "vmcode-interp"
  interpolate (Other x)  = x

public export
fromString : String -> Codegen
fromString "chez"          = Chez
fromString "chez-sep"      = ChezSep
fromString "racket"        = Racket
fromString "gambit"        = Gambit
fromString "node"          = Node
fromString "javascript"    = JavaScript
fromString "refc"          = RefC
fromString "vmcode-interp" = VMCode
fromString x               = Other x

||| Data type describing whether to search for an `.ipkg`
||| file in one of the parent directories (the default), not
||| to use an `.ipkg` file at all, or to use the one provided at
||| the command line.
|||
||| This is only relevant when working with Idris source files
||| directly, for instance when loading them into a REPL session.
public export
data WithIpkg : Type where
  ||| Search for an `.ipkg` file in a parent directory.
  ||| If an `.idr` file is provided, the parent directories
  ||| of this file will be searched. If no `.idr` file is
  ||| given, the current working directory will be search,
  ||| which is given as an argument to this constructor.
  Search : (dir : CurDir) -> WithIpkg

  ||| Don't use an `.ipkg` file.
  None   : WithIpkg

  ||| Use the given `.ipkg` file, provided as a command line
  ||| argument.
  Use    : (ipkg : File Abs) -> WithIpkg

||| Data type describing whether `rlwrap` command should be used
||| and which additional arguments should be passed to it.
|||
||| This is basically equivalent to `Maybe CmdArgList`, but
||| separate type disallows confusion with `Maybe` from `MConfig`,
||| i.e. between not being set and set to not to use `rlwrap`.
public export
data RlwrapConfig : Type where
  DoNotUseRlwrap : RlwrapConfig
  UseRlwrap      : CmdArgList -> RlwrapConfig

||| Type-level identity
public export
0 I : Type -> Type
I t = t

||| User-defined configuration
|||
||| @ f : This is used to represent the context of values.
|||       When we use the config as an environment for running pack
|||       programs, the context will be set to `I` (the identity function
|||       for types). This means, all fields will be mandatory.
|||       For updating the configuration from a `pack.toml` file,
|||       we use context `Maybe`, because all values will be optional.
|||
||| @ c : This is the type of commit given for custom packages.
public export
record Config_ (f : Type -> Type) (c : Type) where
  constructor MkConfig
  ||| Package collection to use
  collection   : f DBName

  ||| URL to a custom Idris repo
  ||| This overrides the one from the package collection if not
  ||| set to `Nothing`.
  idrisURL     : Maybe URL

  ||| Custom Idris commit
  ||| This overrides the one from the package collection if not
  ||| set to `Nothing`.
  idrisCommit  : Maybe c

  ||| All Idris commits mentioned in used configs
  allIdrisCommits : List c

  ||| Custom pack repo
  packURL      : Maybe URL

  ||| Custom pack branch to use (default is `main`)
  packCommit   : Maybe c

  ||| Scheme executable to use
  scheme       : f FilePath

  ||| Whether to bootstrap when building Idris2
  bootstrap    : f Bool

  ||| Whether to prompt for a confirmation when
  ||| building or installing a package with custom
  ||| build or install hooks.
  safetyPrompt : f Bool

  ||| Whether to prompt for a confirmation when
  ||| running the garbage collector for removing no longer used
  ||| installation directories.
  gcPrompt : f Bool

  ||| Whether to issue a warning in precense of a local `depends` directory
  warnDepends : f Bool

  ||| Whether to skip tests during collection checking
  skipTests : f Bool

  ||| List of package names, for which we will not issue a safety prompt
  ||| in case of custom `.ipkg` hooks, even if `safetyPrompt` is set
  ||| to `True`
  whitelist    : f (List PkgName)

  ||| Whether to install the library sources as well
  withSrc      : f Bool

  ||| Whether to install the library docs as well
  withDocs     : f Bool

  ||| Whether to use katla to add semantically highlighted source code
  ||| to the library docs.
  useKatla     : f Bool

  ||| The `.ipkg` file to use (if any) when starting a REPL session
  withIpkg     : f WithIpkg

  ||| Whether to use `rlwrap` to run a REPL session
  rlwrap       : f RlwrapConfig

  ||| Any extra arguments to pass to Idris2 executable.
  extraArgs    : f CmdArgList

  ||| Libraries to install automatically
  autoLibs     : f (List PkgName)

  ||| Applications to install automatically
  autoApps     : f (List PkgName)

  ||| Libraries to automatically load in a REPL session
  autoLoad     : f Autoload

  ||| Customizations to the package data base
  custom       : f (SortedMap DBName (SortedMap PkgName $ Package_ c))

  ||| Type of query to run
  queryType    : f (QueryType)

  ||| Verbosity of the Log
  logLevel     : f (LogLevel)

  ||| Codegen to use
  codegen      : f (Codegen)

  ||| Name of output file when compiling Idris source files
  output       : f Body

  ||| Default LogLevels for different commands
  levels       : f (SortedMap String LogLevel)

||| Configuration with mandatory fields.
public export
0 IConfig : Type -> Type
IConfig = Config_ I

||| Configuration with optional fields.
public export
0 MConfig : Type -> Type
MConfig = Config_ Maybe

||| Application config with meta commits not yet resolved.
public export
0 MetaConfig : Type
MetaConfig = IConfig MetaCommit

||| Fully resolved application config.
public export
0 Config : Type
Config = IConfig Commit

||| User-defined adjustments to the application config.
public export
0 UserConfig : Type
UserConfig = MConfig MetaCommit

||| Effectfully convert all custom package descriptions
||| stored in a configuration. This is mainly used to
||| resolve meta commits to mere commits.
export
traverse :  Applicative f
         => (URL -> a -> f b)
         -> (idrisURL : URL)
         -> Config_ I a
         -> f (Config_ I b)
traverse g idrisURL cfg =
  let iurl = fromMaybe idrisURL cfg.idrisURL
      purl = fromMaybe defaultPackRepo cfg.packURL
      cst = traverse (traverse $ traverse g) cfg.custom
      ic  = traverse (g iurl) cfg.idrisCommit
      ics = traverse (g iurl) cfg.allIdrisCommits
      pc  = traverse (g purl) cfg.packCommit
   in [| adj ic ics pc cst |]
    where adj :  (idrisCommit : Maybe b)
              -> (allidrisCommits : List b)
              -> (packCommit  : Maybe b)
              -> SortedMap DBName (SortedMap PkgName $ Package_ b)
              -> Config_ I b
          adj ic ics pc cb =
            { idrisCommit     := ic
            , allIdrisCommits := ics
            , packCommit      := pc
            , custom          := cb
            } cfg

||| This allows us to use a `Config` in scope when we
||| need an auto-implicit `LogLevel` for logging.
export %inline %hint
configToLogRef : (c : Config) => LogRef
configToLogRef = MkLogRef c.logLevel

||| This allows us to use a `MetaConfig` in scope when we
||| need an auto-implicit `LogLevel` for logging.
export %inline %hint
metaConfigToLogRef : (c : MetaConfig) => LogRef
metaConfigToLogRef = MkLogRef c.logLevel

--------------------------------------------------------------------------------
--          Updating the Config
--------------------------------------------------------------------------------

export infixl 8 `mergeRight`

mergeRight : SortedMap k v -> SortedMap k v -> SortedMap k v
mergeRight = mergeWith (\_,v => v)

pkgs : SortedMap PkgName Package
pkgs = fromList $ (\c => (corePkgName c, Core c)) <$> corePkgs

||| Merges the "official" package collection with user
||| defined settings, which will take precedence.
export
allPackages : (c : Config) => (db : DB) => SortedMap PkgName Package
allPackages =
  let all = fromMaybe empty $ lookup All c.custom
      loc = fromMaybe empty $ lookup c.collection c.custom
   in db.packages `mergeRight` all `mergeRight` loc `mergeRight` pkgs

||| Initial config
export
init : (cur : CurDir) => (coll : DBName) -> MetaConfig
init coll = MkConfig {
    collection      = coll
  , idrisURL        = Nothing
  , idrisCommit     = Nothing
  , allIdrisCommits = []
  , packURL         = Nothing
  , packCommit      = Nothing
  , scheme          = "scheme"
  , bootstrap       = True
  , safetyPrompt    = True
  , gcPrompt        = True
  , warnDepends     = True
  , skipTests       = False
  , whitelist       = []
  , withSrc         = False
  , withDocs        = False
  , useKatla        = False
  , withIpkg        = Search cur
  , rlwrap          = DoNotUseRlwrap
  , extraArgs       = []
  , autoLibs        = []
  , autoApps        = []
  , autoLoad        = NoPkgs
  , custom          = empty
  , queryType       = NameOnly
  , logLevel        = Warning
  , codegen         = Default
  , output          = "_tmppack"
  , levels          = empty
  }

export infixl 7 `update`

||| Update a config with optional settings
export
update : IConfig c -> MConfig c -> IConfig c
update ci cm = MkConfig {
    collection      = fromMaybe ci.collection cm.collection
  , idrisURL        = cm.idrisURL <|> ci.idrisURL
  , idrisCommit     = cm.idrisCommit <|> ci.idrisCommit
  , allIdrisCommits = cm.allIdrisCommits <+> ci.allIdrisCommits
  , packURL         = cm.packURL <|> ci.packURL
  , packCommit      = cm.packCommit <|> ci.packCommit
  , scheme          = fromMaybe ci.scheme cm.scheme
  , bootstrap       = fromMaybe ci.bootstrap cm.bootstrap
  , safetyPrompt    = fromMaybe ci.safetyPrompt cm.safetyPrompt
  , gcPrompt        = fromMaybe ci.gcPrompt cm.gcPrompt
  , warnDepends     = fromMaybe ci.warnDepends cm.warnDepends
  , skipTests       = fromMaybe ci.warnDepends cm.warnDepends
  , withSrc         = fromMaybe ci.withSrc cm.withSrc
  , withDocs        = fromMaybe ci.withDocs cm.withDocs
  , useKatla        = fromMaybe ci.useKatla cm.useKatla
  , withIpkg        = fromMaybe ci.withIpkg cm.withIpkg
  , rlwrap          = fromMaybe ci.rlwrap cm.rlwrap
  , extraArgs       = ci.extraArgs <+> fromMaybe [] cm.extraArgs
  , whitelist       = sortedNub (ci.whitelist ++ concat cm.whitelist)
  , autoLibs        = sortedNub (ci.autoLibs ++ concat cm.autoLibs)
  , autoApps        = sortedNub (ci.autoApps ++ concat cm.autoApps)
  , autoLoad        = fromMaybe ci.autoLoad cm.autoLoad
  , custom          = mergeWith mergeRight ci.custom (fromMaybe empty cm.custom)
  , queryType       = fromMaybe ci.queryType cm.queryType
  , logLevel        = fromMaybe ci.logLevel cm.logLevel
  , codegen         = fromMaybe ci.codegen cm.codegen
  , output          = fromMaybe ci.output cm.output
  , levels          = mergeWith (\_,v => v) ci.levels (fromMaybe empty cm.levels)
  }

--------------------------------------------------------------------------------
--          HasIdris
--------------------------------------------------------------------------------

||| Proof that we have verified that the required Idris
||| compiler has been installed.
public export
data HasIdris : Config -> DB -> Type where
  [noHints]
  ItHasIdris : {0 c : _} -> {0 db : _} -> HasIdris c db

--------------------------------------------------------------------------------
--          Env
--------------------------------------------------------------------------------

||| Cache used during package resolution
public export
0 LibCache : Type
LibCache = IORef (SortedMap PkgName $ ResolvedLib U)

||| Create an empty library cache
export
emptyCache : HasIO io => io LibCache
emptyCache = newIORef SortedMap.empty

||| Cache a resolved library
export
cacheLib :  HasIO io
         => (ref : LibCache)
         => PkgName
         -> ResolvedLib U
         -> io (ResolvedLib U)
cacheLib n lib = modifyIORef ref (insert n lib) $> lib

||| Lookup a library in the cache
export
lookupLib :  HasIO io
          => (ref : LibCache)
          => PkgName
          -> io (Maybe $ ResolvedLib U)
lookupLib n = lookup n <$> readIORef ref

||| Lookup a library in the cache
export
uncacheLib : HasIO io => (ref : LibCache) => PkgName -> io ()
uncacheLib n = modifyIORef ref (delete n)

||| Environment used by most pack methods, consisting of
||| the `PACK_DIR` environment variable, the user-defined
||| application configuratin, and the data collection to
||| use.
public export
record Env where
  constructor MkEnv
  packDir : PackDir
  tmpDir  : TmpDir
  config  : Config
  cache   : LibCache
  db      : DB
  linebuf : LineBufferingCmd

||| This allows us to use an `Env` in scope when we
||| need an auto-implicit `PackDir`.
export %inline %hint
envToPackDir : (e : Env) => PackDir
envToPackDir = e.packDir

||| This allows us to use an `Env` in scope when we
||| need an auto-implicit `PackDir`.
export %inline %hint
envToTmpDir : (e : Env) => TmpDir
envToTmpDir = e.tmpDir

||| This allows us to use an `Env` in scope when we
||| need an auto-implicit `LibCache`.
export %inline %hint
envToCache : (e : Env) => LibCache
envToCache = e.cache

||| This allows us to use an `Env` in scope when we
||| need an auto-implicit `Config`.
export %inline %hint
envToConfig : (e : Env) => Config
envToConfig = e.config

||| This allows us to use an `Env` in scope when we
||| need an auto-implicit `DB`.
export %inline %hint
envToDB : (e : Env) => DB
envToDB = e.db

||| This allows us to use an `Env` in scope when we
||| need an auto-implicit `LineBufferingCmd`.
export %inline %hint
envToLinebuf : (e : Env) => LineBufferingCmd
envToLinebuf = e.linebuf

||| Like `Pack.Config.Types.Env`, but with an erased proof
||| that the Idris compiler installation has been verified.
public export
record IdrisEnv where
  constructor MkIdrisEnv
  env   : Env
  ttc   : TTCVersion
  0 prf : HasIdris env.config env.db

||| This allows us to use an `IdrisEnv` in scope when we
||| need an auto-implicit `Env`.
export %inline %hint
idrisEnvToEnv : (e : IdrisEnv) => Env
idrisEnvToEnv = e.env

||| This allows us to use an `IdrisEnv` in scope when we
||| need an auto-implicit `TTCVersion`.
export %inline %hint
idrisEnvToTTC : (e : IdrisEnv) => TTCVersion
idrisEnvToTTC = e.ttc

--------------------------------------------------------------------------------
--          Command Arguments
--------------------------------------------------------------------------------

||| An interface for parsing the argument list of a pack command
public export
interface Arg (0 a : Type) where
  argDesc_ : String
  readArg  : List String -> Maybe (a, List String)

||| Utility version of `argDesc_` with an explicit erased type argument.
public export %inline
argDesc : (0 a : Type) -> Arg a => String
argDesc a = argDesc_ {a}

||| Utility for implementing `readArg` via a function reading a single string.
export
parseSingleMaybe :  (read : String -> Maybe a)
                 -> List String
                 -> Maybe (a, List String)
parseSingleMaybe read []       = Nothing
parseSingleMaybe read (h :: t) = (,t) <$> read h

||| Utility for implementing `readArg` via a function reading a single string.
export %inline
parseSingle :  (read : String -> Either e a)
            -> List String
            -> Maybe (a, List String)
parseSingle read = parseSingleMaybe (eitherToMaybe . read)

||| Utility for implementing `readArg` via a function reading a single string.
export %inline
readSingle :  (read : String -> a) -> List String -> Maybe (a, List String)
readSingle read = parseSingleMaybe (Just . read)

export
Arg a => Arg (Maybe a) where
  argDesc_ = "[\{argDesc a}]"

  readArg ss = case readArg {a} ss of
    Nothing      => Just (Nothing, ss)
    Just (v,ss') => Just (Just v, ss')

export
(cd : CurDir) => Arg (File Abs) where
  argDesc_ = "<file>"
  readArg = parseSingle (readAbsFile curDir)

export
(cd : CurDir) => Arg (Path Abs) where
  argDesc_ = "<dir>"
  readArg = readSingle (\s => parse s curDir)

export
(cd : CurDir) => Arg PkgOrIpkg where
  argDesc_ = "<pkg or .ipkg>"
  readArg = readSingle $ \s => case readAbsFile curDir s of
    Left  _ => Pkg $ MkPkgName s
    Right f =>
      if isIpkgBody f.file
           then Ipkg f
           else Pkg $ MkPkgName s

export %inline
Arg PkgName where
  argDesc_ = "<pkg>"
  readArg = readSingle MkPkgName

export %inline
Arg PkgType where
  argDesc_ = "<lib | bin>"
  readArg = parseSingle readPkgType

export %inline
Arg CmdArgList where
  argDesc_ = "[<args>]"
  readArg ss = Just (fromStrList ss, [])

export %inline
Arg (List PkgName) where
  argDesc_ = "[<pkgs>]"
  readArg ss = Just (map MkPkgName ss, [])

export %inline
Arg Body where
  argDesc_ = "<file name>"
  readArg = parseSingle readBody

export %inline
Arg DBName where
  argDesc_ = "<db>"
  readArg = parseSingle readDBName

export %inline
Arg String where
  argDesc_ = "<str>"
  readArg = readSingle id

--------------------------------------------------------------------------------
--          Command
--------------------------------------------------------------------------------

||| Interface representing pack commands. We abstract over this
||| because both pack and pack-admin accept different types of
||| commands, but both use the same functionality for reading
||| the application config based on the command they use.
|||
||| A command `c` is expected to be an enum type. This interface provides
||| a name and detailed description for each command, as well as the types of
||| arguments a command takes.
|||
||| This allows us to generate useful error messages when the wrong type
||| of argument is passed to a command. It also allows us to implement the
||| parsing of commands only once.
public export
interface Command c where
  ||| The command to use if only command line options but
  ||| not command is given.
  defaultCommand : c

  ||| Name of the application in question
  appName : String

  ||| Name of command in question
  cmdName : c -> String

  ||| The default log level to use.
  defaultLevel    : c -> LogLevel

  ||| Detailed usage description of the command
  desc : c -> String

  ||| General usage of the application in question
  usage : Lazy String

  ||| Types of arguments required by the given command
  0 ArgTypes : c -> List Type

  ||| Tries to read a command from a String
  readCommand_ : String -> Maybe c

  ||| List of argument readers for the current command
  readArgs : CurDir => (cmd : c) -> All Arg (ArgTypes cmd)

  ||| Some commands overwrite certain aspects of the user-defined
  ||| config. For instance, `pack switch latest` must overwrite the
  ||| package collection read from the `pack.toml` files with the
  ||| latest package collection available.
  adjConfig_ :  HasIO io
             => PackDir
             => TmpDir
             => (cmd : c)
             -> All I (ArgTypes cmd)
             -> MetaConfig
             -> EitherT PackErr io MetaConfig

args : All Arg ts -> CurDir => List String -> Maybe (All I ts)
args [] []       = Just []
args [] (_ :: _) = Nothing
args {ts = x :: xs} (p :: ps) ss = do
  (v,ss') <- readArg {a = x} ss
  vs      <- args ps ss'
  pure (v :: vs)

argsDesc : CurDir => Command c => Maybe c -> String
argsDesc Nothing  = " [<args>]"
argsDesc (Just x) = fastConcat $ go (readArgs x)
  where go : All Arg ts -> List String
        go = forget . mapProperty (\p => " " ++ argDesc_ @{p})

||| Header line for a usage string
export
usageHeader : CurDir => Command c => Maybe c -> String
usageHeader cmd =
  let nm      := maybe "<cmd>" cmdName cmd
   in "Usage: \{appName {c}} [options] \{nm}\{argsDesc cmd}"

ind : String -> String
ind = unlines . map (indent 2) . lines

||| Detailed description how to use the given command.
|||
||| This is a general description of the application
||| in case the argument is `Nothing`.
export
usageDesc : CurDir => Command c => Maybe c -> String
usageDesc m = case m of
  Just cmd =>
    """
    \{usageHeader m}

    \{ind $ desc cmd}
    """

  Nothing =>
    """
    \{usageHeader m}

    \{usage {c}}
    """

||| Type of arguments expected by the given command
public export
0 Args : Command c => c -> Type
Args cmd = All I (ArgTypes cmd)

||| A pack command together with its list of arguments
public export
0 CommandWithArgs : (c : Type) -> Command c => Type
CommandWithArgs c = DPair c Args

readCWA :  Command c
        => CurDir
        => (cmd : c)
        -> List String
        -> Either PackErr (CommandWithArgs c)
readCWA x ss =
  let readers := readArgs x
      Just as := args readers ss
        | Nothing => Left (InvalidCmdArgs (cmdName x) ss $ usageHeader $ Just x)
   in Right (x ** as)

||| Convenience alias for `readCommand_` with an explicit
||| erased argument for the command type.
export
readCommand :  (0 c : Type)
            -> Command c
            => CurDir
            -> List String
            -> Either PackErr (CommandWithArgs c)
readCommand c cd []       = readCWA defaultCommand []
readCommand c cd (h :: t) = case readCommand_ {c} h of
  Nothing => Left (UnknownCommand h $ usageDesc {c} Nothing)
  Just x  => readCWA x t

||| Some commands overwrite certain aspects of the user-defined
||| config. For instance, `pack switch latest` must overwrite the
||| package collection read from the `pack.toml` files with the
||| latest package collection available.
export %inline
adjConfig :  HasIO io
          => Command c
          => PackDir
          => TmpDir
          => CommandWithArgs c
          -> MetaConfig
          -> EitherT PackErr io MetaConfig
adjConfig (command ** args) = adjConfig_ command args
