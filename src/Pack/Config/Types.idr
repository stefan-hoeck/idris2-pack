module Pack.Config.Types

import Data.List
import Data.Maybe
import Data.SortedMap as SM
import Idris.Package.Types
import Libraries.Data.List.Extra
import Pack.Core
import Pack.Database.Types

%default total

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

  ||| Custom pack repo
  packURL      : Maybe URL

  ||| Custom pack branch to use (default is `main`)
  packCommit   : Maybe c

  ||| Scheme executable to use
  scheme       : f FilePath

  ||| Whether to prompt for a confirmation when
  ||| building or installing a package with custom
  ||| build or install hooks.
  safetyPrompt : f Bool

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
  rlwrap       : f Bool

  ||| Libraries to install automatically
  autoLibs     : f (List PkgName)

  ||| Applications to install automatically
  autoApps     : f (List PkgName)

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
      pc  = traverse (g purl) cfg.packCommit
   in [| adj ic pc cst |]
    where adj :  (idrisCommit : Maybe b)
              -> (packCommit  : Maybe b)
              -> SortedMap DBName (SortedMap PkgName $ Package_ b)
              -> Config_ I b
          adj ic pc cb = {idrisCommit := ic, packCommit := pc, custom := cb} cfg

||| This allows us to use a `Config` in scope when we
||| need an auto-implicit `LogLevel` for logging.
export %inline %hint
configToLogLevel : (c : Config) => LogLevel
configToLogLevel = c.logLevel

||| This allows us to use a `MetaConfig` in scope when we
||| need an auto-implicit `LogLevel` for logging.
export %inline %hint
metaConfigToLogLevel : (c : MetaConfig) => LogLevel
metaConfigToLogLevel = c.logLevel

--------------------------------------------------------------------------------
--          Updating the Config
--------------------------------------------------------------------------------

infixl 8 `mergeRight`

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
    collection   = coll
  , idrisURL     = Nothing
  , idrisCommit  = Nothing
  , packURL      = Nothing
  , packCommit   = Nothing
  , scheme       = "scheme"
  , safetyPrompt = True
  , withSrc      = False
  , withDocs     = False
  , useKatla     = False
  , withIpkg     = Search cur
  , rlwrap       = False
  , autoLibs     = []
  , autoApps     = []
  , custom       = empty
  , queryType    = NameOnly
  , logLevel     = Warning
  , codegen      = Default
  , output       = "_tmppack"
  }

infixl 7 `update`

||| Update a config with optional settings
export
update : IConfig c -> MConfig c -> IConfig c
update ci cm = MkConfig {
    collection   = fromMaybe ci.collection cm.collection
  , idrisURL     = cm.idrisURL <|> ci.idrisURL
  , idrisCommit  = cm.idrisCommit <|> ci.idrisCommit
  , packURL      = cm.packURL <|> ci.packURL
  , packCommit   = cm.packCommit <|> ci.packCommit
  , scheme       = fromMaybe ci.scheme cm.scheme
  , safetyPrompt = fromMaybe ci.safetyPrompt cm.safetyPrompt
  , withSrc      = fromMaybe ci.withSrc cm.withSrc
  , withDocs     = fromMaybe ci.withDocs cm.withDocs
  , useKatla     = fromMaybe ci.useKatla cm.useKatla
  , withIpkg     = fromMaybe ci.withIpkg cm.withIpkg
  , rlwrap       = fromMaybe ci.rlwrap cm.rlwrap
  , autoLibs     = sortedNub (ci.autoLibs ++ concat cm.autoLibs)
  , autoApps     = sortedNub (ci.autoApps ++ concat cm.autoApps)
  , custom       = mergeWith mergeRight ci.custom (fromMaybe empty cm.custom)
  , queryType    = fromMaybe ci.queryType cm.queryType
  , logLevel     = fromMaybe ci.logLevel cm.logLevel
  , codegen      = fromMaybe ci.codegen cm.codegen
  , output       = fromMaybe ci.output cm.output
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
  db      : DB

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
||| need an auto-implicit `Config`.
export %inline %hint
envToConfig : (e : Env) => Config
envToConfig = e.config

||| This allows us to use an `Env` in scope when we
||| need an auto-implicit `DB`.
export %inline %hint
envToDB : (e : Env) => DB
envToDB = e.db

||| Like `Pack.Config.Types.Env`, but with an erased proof
||| that the Idris compiler installation has been verified.
public export
record IdrisEnv where
  constructor MkIdrisEnv
  env   : Env
  0 prf : HasIdris env.config env.db

||| This allows us to use an `IdrisEnv` in scope when we
||| need an auto-implicit `Env`.
export %inline %hint
idrisEnvToEnv : (e : IdrisEnv) => Env
idrisEnvToEnv = e.env

--------------------------------------------------------------------------------
--          Command
--------------------------------------------------------------------------------

||| Interface representing pack commands. We abstract over this
||| because both pack and pack-admin accept different types of
||| commands, but both use the same functionality for reading
||| the application config based on the command they use.
public export
interface Command c where
  ||| The command to use if only command line options but
  ||| not command is given.
  defaultCommand_ : c

  ||| The default log level to use.
  defaultLevel    : c -> LogLevel

  ||| Some commands overwrite certain aspects of the user-defined
  ||| config. For instance, `pack switch latest` must overwrite the
  ||| package collection read from the `pack.toml` files with the
  ||| latest package collection available.
  adjConfig :  HasIO io
            => PackDir
            => TmpDir
            => c
            -> MetaConfig
            -> EitherT PackErr io MetaConfig

  ||| Tries to read a command from a list of command line arguments.
  readCommand_ : CurDir -> List String -> Either PackErr c

||| Convenience alias for `defaultCommand_` with an explicit
||| erased argument for the command type.
export %inline
defaultCommand : (0 c : Type) -> Command c => c
defaultCommand _ = defaultCommand_

||| Convenience alias for `readCommand_` with an explicit
||| erased argument for the command type.
export %inline
readCommand :  (0 c : Type)
            -> Command c
            => CurDir
            -> List String
            -> Either PackErr c
readCommand _ = readCommand_
