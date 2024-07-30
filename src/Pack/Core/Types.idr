||| We work a lot with Strings of distinct semantics.
||| Since I've been bitten by this more than once, we wrap
||| the in single field record types to drastically increase
||| type safety.
module Pack.Core.Types

import public Data.FilePath.File
import Data.Either
import Data.Maybe
import Idris.Package.Types
import System.File

%default total

----------------------------------------------------------------------------------
----          Quoted Strings
----------------------------------------------------------------------------------

||| This puts a value in quotes during interpolation.
|||
||| Note: If the interpolated string contains additional quote
||| characters, these will *not* be escaped.
export
quote : Interpolation a => a -> String
quote v = "\"\{v}\""

----------------------------------------------------------------------------------
----          Paths
----------------------------------------------------------------------------------

||| Returns the second path, relative to the first one
||| For instance, `relativeTo /foo/bar/baz /foo/quux` will return
||| `../../quux`.
export
relativeTo : (origin, target : Path Abs) -> Path Rel
relativeTo (PAbs sx) (PAbs sy) = PRel $ go (sx <>> []) (sy <>> [])

  where
    go : (o,t : List Body) -> SnocList Body
    go [>]       t  = Lin <>< t
    go xs        [] = Lin <>< xs $> ".."
    go (x :: xs) (y :: ys) = case x == y of
      True  => go xs ys
      False => Lin <>< (((x :: xs) $> "..") ++ (y :: ys))

||| True if the given file path body ends on `.ipkg`
export
isIpkgBody : Body -> Bool
isIpkgBody = (Just "ipkg" ==) . extension

||| True if the given file path body ends on `.toml`
export
isTomlBody : Body -> Bool
isTomlBody = (Just "toml" ==) . extension

||| True if the given file path body ends on `.html`
export
isHtmlBody : Body -> Bool
isHtmlBody = (Just "html" ==) . extension

toRelPath : String -> Path Rel
toRelPath s = case the FilePath (fromString s) of
  FP (PAbs sx) => PRel sx
  FP (PRel sx) => PRel sx

||| Converts a `FilePath` - which might hold a relative
||| or an absolute path - to an absolute path by interpreting
||| a relative path being relative to the given parent directory.
export
toAbsPath : (parent : Path Abs) -> FilePath -> Path Abs
toAbsPath parent (FP $ PAbs sx) = PAbs sx
toAbsPath parent (FP $ PRel sx) = parent </> PRel sx

||| Parses a string, converting it to either a relative
||| or absolute path and using `toAbsPath` to convert the result
||| to an absolute path.
export
parse : String -> (parentDir : Path Abs) -> Path Abs
parse s parentDir = toAbsPath parentDir (fromString s)

export %inline
Cast String (Path Rel) where
  cast = toRelPath

export infixl 5 <//>

||| More flexible version of `</>` (path concatenation).
export %inline
(<//>) : Cast a (Path Rel) => Path t -> a -> Path t
p <//> v = p </> cast v

export infixl 5 //>

||| More flexible version of `//>`
||| (appending a file path body to an absolute path)
export %inline
(//>) : Cast a Body => Path t -> a -> Path t
p //> v = p /> cast v

export infixl 8 <->

||| Concatenate two file path bodies with a hyphen inbetween.
export
(<->) : Cast a Body => Cast b Body => a -> b -> Body
x <-> y = the Body (cast x) <+> "-" <+> cast y

||| Convert a package version to a file path body.
export
Cast PkgVersion Body where
  cast = fromMaybe "0" . parse . show

||| Convert a package version to a file path body.
export
Cast (Maybe PkgVersion) Body where
  cast Nothing  = "0"
  cast (Just v) = cast v

||| Convert a relative file path to an absolute one by appending
||| it to the given parent directory.
export %inline
toAbsFile : Path Abs -> File Rel -> File Abs
toAbsFile parentDir (MkF p f) = MkF (parentDir </> p) f

export
Cast (File t) (Path t) where cast = toPath

----------------------------------------------------------------------------------
----          CurDir, PackDir, and TmpDir
----------------------------------------------------------------------------------

||| The directory where package collections, global user settings,
||| and cached `.ipkg` files are stored.
public export
data PackDir : Type where
  [noHints]
  PD : (dir : Path Abs) -> PackDir

export %inline
Interpolation PackDir where
  interpolate (PD dir) = interpolate dir

||| Use this when you need access to the `PACK_DIR` path with
||| only a value of type `PackDir` in scope.
export %inline
packDir : (pd : PackDir) => Path Abs
packDir {pd = PD dir} = dir

||| The directory from which the pack application was invoked.
public export
data CurDir : Type where
  [noHints]
  CD : (dir : Path Abs) -> CurDir

export %inline
Interpolation CurDir where
  interpolate (CD dir) = interpolate dir

||| Use this when you need access to the current directory's path with
||| only a value of type `CurDir` in scope.
export %inline
curDir : (cd : CurDir) => Path Abs
curDir {cd = CD dir} = dir

||| The directory where temporary files and git repos will be
||| kept.
public export
data TmpDir : Type where
  [noHints]
  TD : (dir : Path Abs) -> TmpDir

export %inline
Interpolation TmpDir where
  interpolate (TD dir) = interpolate dir

||| Use this when you need access to the `PACK_DIR` path with
||| only a value of type `PackDir` in scope.
export %inline
tmpDir : (td : TmpDir) => Path Abs
tmpDir {td = TD dir} = dir

----------------------------------------------------------------------------------
----          TTC-Version
----------------------------------------------------------------------------------

||| The TTC-Version currently used by the Idris compiler
public export
record TTCVersion where
  [noHints]
  constructor TTCV
  version : Maybe Body

||| Use this when you need access to the current TTC version
||| only a value of type `TTCVersion` in scope.
export %inline
ttcVersion : (ttc : TTCVersion) => Maybe Body
ttcVersion = ttc.version

----------------------------------------------------------------------------------
----          Interpolation
----------------------------------------------------------------------------------

||| Convenience implementation for printing file errors in string
||| interpolation
export
Interpolation FileError where interpolate = show

||| Convenience implementation for printing package versions in string
||| interpolation
export
Interpolation PkgVersion where interpolate = show

--------------------------------------------------------------------------------
--          URL
--------------------------------------------------------------------------------

||| URL mostly used to represent Git repositories.
public export
record URL where
  constructor MkURL
  value : String

export %inline
Eq URL where (==) = (==) `on` value

export %inline
FromString URL where fromString = MkURL

export %inline
Interpolation URL where interpolate = value

export
Cast URL (Path Rel) where
  cast (MkURL s) = case unpack s of
    'h'::'t'::'t'::'p'::'s'::':'::'/'::'/':: t => cast (pack t)
    'h'::'t'::'t'::'p'::':'::'/'::'/':: t      => cast (pack t)
    t                                          => cast (pack t)

--------------------------------------------------------------------------------
--          Commits
--------------------------------------------------------------------------------

||| A git commit hash or tag.
public export
record Commit where
  constructor MkCommit
  value : String

export %inline
Eq Commit where (==) = (==) `on` value

export %inline
FromString Commit where fromString = MkCommit

export %inline
Interpolation Commit where interpolate = value

export %inline
Cast Commit (Path Rel) where
  cast = toRelPath . value

--------------------------------------------------------------------------------
--          Branches
--------------------------------------------------------------------------------

||| A branch in a git repo.
public export
record Branch where
  constructor MkBranch
  value : String

export %inline
Eq Branch where (==) = (==) `on` value

export %inline
FromString Branch where fromString = MkBranch

export %inline
Interpolation Branch where interpolate = value

export %inline
Cast Branch (Path Rel) where
  cast = toRelPath . value

--------------------------------------------------------------------------------
--          Package Name
--------------------------------------------------------------------------------

||| Name of an Idris package
public export
record PkgName where
  constructor MkPkgName
  value : String

export %inline
Eq PkgName where (==) = (==) `on` value

export %inline
Ord PkgName where compare = compare `on` value

export %inline
FromString PkgName where fromString = MkPkgName

export %inline
Interpolation PkgName where interpolate = value

export %inline
Cast PkgName (Path Rel) where
  cast = toRelPath . value

--------------------------------------------------------------------------------
--          PkgOrIpkg
--------------------------------------------------------------------------------

||| Several pack commands operat either on a pack package or a local
||| `.ipkg` file. This data type represents such command line arguments.
public export
data PkgOrIpkg : Type where
  Pkg :  PkgName -> PkgOrIpkg
  Ipkg : File Abs -> PkgOrIpkg

--------------------------------------------------------------------------------
--          Package Type
--------------------------------------------------------------------------------

||| Type of an Idris package (either a library or a binary).
public export
data PkgType = PLib | PApp

export
Interpolation PkgType where
  interpolate PLib = "lib"
  interpolate PApp = "app"

export
Eq PkgType where
  PLib == PLib = True
  PApp == PApp = True
  _    == _    = False

export
Ord PkgType where
  compare PLib PApp = LT
  compare PApp PLib = GT
  compare _    _    = EQ

--------------------------------------------------------------------------------
--          Install Type
--------------------------------------------------------------------------------

||| What we want to install: A library, an application to
||| run it from within pack, or an application, which should
||| be available via the `PATH` variable.
public export
data InstallType : Type where
  Library : InstallType
  App     : (withWrapperScript : Bool) -> InstallType

export %inline
Interpolation InstallType where
  interpolate Library = "library"
  interpolate (App b) = "app"

export
Eq InstallType where
  Library == Library = True
  App b1  == App b2  = b1 == b2
  _       == _       = False

export
Ord InstallType where
  compare Library  (App _)  = LT
  compare (App _)  Library  = GT
  compare Library  Library  = EQ
  compare (App b1) (App b2) = compare b1 b2

--------------------------------------------------------------------------------
--          DBName
--------------------------------------------------------------------------------

||| Name of a package collection. This must be a valid
||| file path body.
public export
record DBName where
  constructor MkDBName
  value : Body

export %inline
Eq DBName where (==) = (==) `on` value

export %inline
Ord DBName where compare = compare `on` value

export %inline
Interpolation DBName where interpolate = interpolate . value

export %inline
Cast DBName Body where
  cast = value

export %inline
Head : DBName
Head = MkDBName "HEAD"

export %inline
All : DBName
All = MkDBName "all"

--------------------------------------------------------------------------------
--          Desc
--------------------------------------------------------------------------------

||| A tagged package desc. We use the tag mainly to make sure that
||| the package desc in question has been checked for safety issues.
||| Since the tag is parameterized by a `PkgDesc`, we make sure
||| we will not inadvertently use a tag for a `PkgDesc` different to
||| the one we wrapped.
public export
record Desc (t : PkgDesc -> Type) where
  constructor MkDesc
  ||| The parsed package desc
  desc : PkgDesc

  ||| String content of the `.ipkg` file used when reading the package desc
  cont : String

  ||| Path to the file use when reading the package desc
  path : File Abs

  ||| Security tag. See `Pack.Runner.Database.check`
  0 tag : t desc

||| This is used as a tag for unchecked `Desc`s.
public export
0 U : PkgDesc -> Type
U d = Unit

namespace PkgDesc
  ||| Lists the dependencies of a package.
  export
  dependencies : PkgDesc -> List PkgName
  dependencies d = map (MkPkgName . pkgname) $ d.depends

||| Lists the dependencies of a package.
export
dependencies : Desc t -> List PkgName
dependencies d = dependencies d.desc

--------------------------------------------------------------------------------
--          CmdArg
--------------------------------------------------------------------------------

||| A command line argument of a call to the system shell.
public export
data CmdArg : Type where

  ||| Escapable alternative represents a string that should be understood as a
  ||| single argument disregarding its contents, i.e. the whole string should
  ||| be passed as an argument even if it contains spaces or characters that
  ||| are normally understood specially by shell.
  Escapable : String -> CmdArg

  ||| No escape alternative represents a raw string that is used "as is" at the
  ||| shell call, thus allowing to pass commands to the shell itself.
  ||| Be aware that spaces inside these strings would form several actual
  ||| arguments and quotation marks and backslash symbols may interfere with
  ||| escaping of other arguments.
  NoEscape : String -> CmdArg

||| Interface that is used to mark which types can be used as a source for
||| command line arguments when building lists of such.
||| This allows to be able to pass different literals and expressions without
||| explicit wrapping them into the `CmdArg` type.
public export
interface CmdArgable a where
  toCmdArg : a -> CmdArg

export %inline
CmdArgable CmdArg where
  toCmdArg = id

||| Implementation that allows any type implementing `Interpolation` interface
||| to be used as an escapable command line argument.
export %inline
Interpolation a => CmdArgable a where
  toCmdArg = Escapable . interpolate

||| A list of command line arguments.
|||
||| This type is meant to look syntacitcally as a simple list, however
||| containing possibly values of different types which can form a command
||| line argument.
|||
||| For example, a call to some command with redirection may look like this:
||| `sys ["echo", "a", NoEscape ">", file]`,
||| when, say, `file` is a value of type `File Abs`.
public export
data CmdArgList : Type where
  Nil  : CmdArgList
  (::) : CmdArgable a => a -> CmdArgList -> CmdArgList

cmdArgList : CmdArgList -> List CmdArg
cmdArgList []      = []
cmdArgList (x::xs) = toCmdArg x :: cmdArgList xs

||| Converts a list of command line arguments to a single string
||| while putting spaces between arguments and escaping appropriate ones.
|||
||| For example, call `escapeCmd ["echo", "&&", NoEscape "&&", "echo", "yes"]`
||| would return a string equivalent to the literal
||| `#""echo" "&&" && "echo" "yes""#`.
export
escapeCmd : CmdArgList -> String
escapeCmd = unwords . map manageArg . cmdArgList where
  manageArg : CmdArg -> String
  manageArg $ Escapable s = escapeArg s
  manageArg $ NoEscape s  = s

namespace CmdArg

  ||| Concatenate two command line arguments into one.
  |||
  ||| This operation respects meaning of the contents of each argument,
  ||| whether they should be escaped or not.
  ||| This allows, say, to form an argument which partially contains something
  ||| escapable and partially contains a special shell argument, for example
  ||| `sys ["cp", "-r", Escapable "\{dirName}/" ++ NoEscape "*", dest]` would
  ||| list files with shell's `*` even if `dirName` contains spaces.
  export
  (++) : CmdArg -> CmdArg -> CmdArg
  Escapable x ++ Escapable y = Escapable $ x ++ y
  Escapable x ++ NoEscape y  = NoEscape $ escapeArg x ++ y
  NoEscape x  ++ Escapable y = NoEscape $ x ++ escapeArg y
  NoEscape x  ++ NoEscape y  = NoEscape $ x ++ y

namespace CmdArgList

  ||| Concatenation operation for command line argument lists.
  export
  (++) : CmdArgList -> CmdArgList -> CmdArgList
  []      ++ ys = ys
  (x::xs) ++ ys = x :: xs ++ ys

  export
  Semigroup CmdArgList where
    (<+>) = (++)

  export
  Monoid CmdArgList where
    neutral = []

  ||| Specialised version of `concatMap` from `Foldable` for `CmdArgList`,
  ||| since `CmdArgList` cannot implement `Foldable`.
  export
  concatMap : Monoid m => (CmdArg -> m) -> CmdArgList -> m
  concatMap f []      = neutral
  concatMap f (x::xs) = f (toCmdArg x) <+> concatMap f xs

  ||| Function that forms a command line arguments list from a raw list of
  ||| strings, treating each string as a single escapable argument.
  export
  fromStrList : List String -> CmdArgList
  fromStrList = foldr (\x, xs => x::xs) Nil

--------------------------------------------------------------------------------
--          LineBufferingCmd
--------------------------------------------------------------------------------

||| A wrapper for a command list of a shell command which allows to do
||| line buffering.
public export
record LineBufferingCmd where
  [noHints]
  constructor MkLineBufferingCmd
  lineBufferingCmd : CmdArgList

--------------------------------------------------------------------------------
--          Logging
--------------------------------------------------------------------------------

||| Level used during logging.
public export
data LogLevel : Type where
  [noHints]
  Debug   : LogLevel
  Build   : LogLevel
  Info    : LogLevel
  Cache   : LogLevel
  Warning : LogLevel
  Silence : LogLevel

llToNat : LogLevel -> Nat
llToNat Debug   = 0
llToNat Build   = 1
llToNat Info    = 2
llToNat Cache   = 3
llToNat Warning = 4
llToNat Silence = 5

export
Eq LogLevel where (==) = (==) `on` llToNat

export
Ord LogLevel where compare = compare `on` llToNat

export
Interpolation LogLevel where
  interpolate Debug   = "debug"
  interpolate Build   = "build"
  interpolate Info    = "info"
  interpolate Cache   = "cache"
  interpolate Warning = "warning"
  interpolate Silence = ""

export
logLevels : List (String, LogLevel)
logLevels =
  [ ("debug"  , Debug  )
  , ("build"  , Build  )
  , ("info"   , Info   )
  , ("cache"  , Cache  )
  , ("warning", Warning)
  , ("silence", Silence)
  ]

||| Reference `LogLevel` to be used as an auto implicit
public export
record LogRef where
  [noHints]
  constructor MkLogRef
  level : LogLevel

--------------------------------------------------------------------------------
--          Errors
--------------------------------------------------------------------------------

||| Error during marshalling from TOML to an Idris type.
public export
data TOMLErr : Type where
  ||| A mandatory key/value pair in a toml file is
  ||| missing
  MissingKey : (path : List String) -> TOMLErr

  ||| A value in a toml file has the wrong type
  WrongType : (path  : List String) -> (type  : String) -> TOMLErr

tomlPath : List String -> String
tomlPath = concat . intersperse "."

printTOMLErr : TOMLErr -> String
printTOMLErr (MissingKey path) = "Missing toml key: \{tomlPath path}."
printTOMLErr (WrongType path type) =
  "Wrong type at \{tomlPath path}. Expect \{type}."

||| Prefix the given TOML key to an error message. This allows us to
||| specify exactly where in a TOML structure things went wrong.
export
prefixKey : (key : String) -> Either TOMLErr a -> Either TOMLErr a
prefixKey k = mapFst $ \case
  MissingKey p => MissingKey (k :: p)
  WrongType p t => WrongType (k :: p) t

||| Errors that can occur when running pack programs.
public export
data PackErr : Type where
  ||| Failed to get the path of the current directory.
  NoCurDir   : PackErr

  ||| Failed to get package directory path
  NoPackDir  : PackErr

  ||| Failed to create temporary directory
  NoTmpDir  : PackErr

  ||| Failed to create the given directory
  MkDir      : (path : Path Abs) -> (err : FileError) -> PackErr

  ||| Failed to read the given file
  ReadFile   : (path : File Abs) -> (err : FileError) -> PackErr

  ||| Failed to write to the given file
  WriteFile  : (path : File Abs) -> (err : FileError) -> PackErr

  ||| Failed to read the content of a directory
  DirEntries : (path : Path Abs) -> (err : FileError) -> PackErr

  ||| Error when running the given system command
  Sys        : (cmd : CmdArgList) -> (err : Int) -> PackErr

  ||| Error when changing into the given directory
  ChangeDir  : (path : Path Abs) -> PackErr

  ||| The given package is not in the package data base
  UnknownPkg : (name : PkgName) -> PackErr

  ||| The given package is not a local package
  NotLocalPkg : (name : PkgName) -> PackErr

  ||| The given package is not an applicatio
  ||| (No executable name set in the `.ipkg` file)
  NoApp      : (rep : PkgName) -> PackErr

  ||| The given package is not an application
  ||| (No executable name set in the `.ipkg` file)
  NoAppIpkg  : (path : File Abs) -> PackErr

  ||| An erroneous package description in a package DB file
  InvalidPackageDesc : (s : String) -> PackErr

  ||| The package database is empty (no header)
  EmptyPkgDB : PackErr

  ||| Invalid package database header
  InvalidDBHeader : (s : String) -> PackErr

  ||| Invalid package database header
  InvalidDBName : (s : String) -> PackErr

  ||| Invalid package type
  InvalidPkgType : (s : String) -> PackErr

  ||| Invalid package version
  InvalidPkgVersion : (s : String) -> PackErr

  ||| Invalid log level
  InvalidLogLevel : (s : String) -> PackErr

  ||| Failed to parse an .ipkg file
  InvalidIpkgFile  : (path : File Abs) -> PackErr

  ||| Invalid file path body
  InvalidBody  : (s : String) -> PackErr

  ||| Failed to parse a file path
  NoFilePath : (str : String) -> PackErr

  ||| The given core package (base, contrib, etc.)
  ||| is missing from the Idris installation.
  MissingCorePackage :
       (name    : PkgName)
    -> (version : PkgVersion)
    -> (commit  : Commit)
    -> PackErr

  ||| Unknown command line argument
  UnknownArg : (arg : String) -> PackErr

  ||| Invalid command line arguments (in micropack)
  InvalidArgs : (args : List String) -> PackErr

  ||| Erroneous command line argument
  ErroneousArg : (err : String) -> PackErr

  ||| Trying to run zero or more than one local package
  ||| (or something that isn't a local package).
  BuildMany : List Body -> PackErr

  ||| Unknown pack command
  UnknownCommand : String -> (usage : String) -> PackErr

  ||| Unknown pack command
  InvalidCmdArgs :
       (cmd   : String)
    -> (args  : List String)
    -> (usage : String)
    -> PackErr

  ||| Trying to clone a repository into an existing
  ||| directory.
  DirExists : Path Abs -> PackErr

  ||| Error in a toml file.
  TOMLFile :  (file : File Abs) -> (err : TOMLErr) -> PackErr

  ||| Error in a toml file.
  TOMLParse : (err : String) -> PackErr

  ||| Number of failures when building packages.
  BuildFailures : Nat -> PackErr

  ||| User tried to manually install the pack application
  ManualInstallPackApp : PackErr

  ||| User aborted installation of a lib/app with custom build hooks.
  SafetyAbort : PackErr

||| Prints an error that occured during program execution.
export
printErr : PackErr -> String
printErr NoCurDir = "Failed to get current directory."

printErr NoPackDir = """
  Failed to figure out package directory.
  This means, that neither environment variable \"PACK_DIR\"
  nor environment varaible \"HOME\" was set.
  """

printErr NoTmpDir = """
  Failed to create temporary directory.
  Please check directory `PACK_DIR` and make sure to remove
  all `.tmpXY` directories you no longer need.
  """

printErr (MkDir path err) =
  "Error when creating directory \{quote path}: \{err}."

printErr (ReadFile path err) =
  "Error when reading file \{quote path}: \{err}."

printErr (WriteFile path err) =
  "Error when writing to file \{quote path}: \{err}."

printErr (DirEntries path err) =
  "Error when reading directory \{quote path}: \{err}."

printErr (Sys cmd err) = """
  Error when executing system command.
  Command: \{escapeCmd cmd}
  Error code: \{show err}
  """

printErr (ChangeDir path) =
  "Failed to change to directory \{quote path}."

printErr (InvalidPackageDesc s) = """
  Invalid package description: \{quote s}.
  This should be of the format \"name,url,commit hash,ipkg file\".
  """

printErr (InvalidDBHeader s) = """
  Invalid data base header: \{quote s}.
  This should be of the format \"idris2 commit hash,idris2 version\".
  """

printErr (InvalidDBName s) = """
  Invalid data collection name: \{quote s}.
  This should be a non-empty string without path separators.
  """

printErr (InvalidBody s) = "Invalid file path body: \{quote s}."

printErr (InvalidPkgType s) = """
  Invalid package type: \{quote s}.
  Valid types are `lib` and `bin`.
  """

printErr (InvalidPkgVersion s) = "Invalid package version: \{quote s}."

printErr (InvalidLogLevel s) = """
  Invalid log level: \{quote s}. Valid values are
  \{joinBy "\n" $ ("- " ++) . fst <$> logLevels}
  """

printErr (UnknownPkg name) = "Unknown package: \{name}"

printErr (NotLocalPkg name) = "Not a local package: \{name}"

printErr (NoApp rep) = "Package \{rep} is not an application"

printErr (NoAppIpkg p) = "Package \{p} is not an application"

printErr EmptyPkgDB = "Empty package data base"

printErr (InvalidIpkgFile path) =
  "Failed to parse .ipkg file: \{path}"

printErr (MissingCorePackage nm v c) =
  "Core package \{quote nm} missing for Idris2 version \{v} (commit: \{c})"

printErr (UnknownArg arg) = "Unknown command line arg: \{arg}"

printErr (InvalidArgs args) = "Invalid command line args: \{unwords args}"

printErr (ErroneousArg err) = err

printErr (UnknownCommand cmd usage) =
  """
  Unknown command: "\{cmd}"

  \{usage}
  """

printErr (InvalidCmdArgs cmd args usage) =
  """
  Invalid argument(s) for command `\{cmd}`.

  \{usage}
  """

printErr (BuildMany []) = "No local `.ipkg` files found."
printErr (BuildMany fs@(_::_)) = """
  Ambiguous `.ipkg` files:
  \{joinBy "\n" $ ("- " ++) . interpolate <$> fs}
  Please choose only one.
  """

printErr (NoFilePath s) = "Not a file path : \{s}"

printErr (DirExists path) = """
  Failed to clone Git repository into \{path}.
  Directory already exists.
  """

printErr (TOMLFile file err) =
  "Error in file \{file}: \{printTOMLErr err}."

printErr (TOMLParse err) = err

printErr (BuildFailures 1) = "1 package failed to build."

printErr (BuildFailures n) = "\{show n} packages failed to build."

printErr ManualInstallPackApp = """
  You are not supposed to manually install or remove the pack
  application. In order to update pack to the latest version on
  GitHub, run `pack update`.

  Note: If you didn't run `pack install-app pack` or a similar
  operation, "pack" might be listed as an auto-install application
  in one of your pack.toml files. Please remove it from there.
  """

printErr SafetyAbort = "Aborted."

||| Tries to convert a string to a `DBName` by first converting
||| it to a valid file path body.
export
readDBName : String -> Either PackErr DBName
readDBName s = case Body.parse s of
  Just b  => Right $ MkDBName b
  Nothing => Left (InvalidDBName s)

||| Tries to convert a string to a file path body.
export
readBody : String -> Either PackErr Body
readBody s = case Body.parse s of
  Just b  => Right b
  Nothing => Left (InvalidBody s)

||| Tries to convert a string to a package type.
export
readPkgType : String -> Either PackErr PkgType
readPkgType "lib" = Right PLib
readPkgType "bin" = Right PApp
readPkgType "app" = Right PApp
readPkgType s     = Left (InvalidPkgType s)

||| Tries to convert a string representing a relative or absolute
||| file path. This uses `toAbsPath` internally, so it is somewhat
||| tolerant w.r.t. to dubious file paths. It fails, however, if the
||| given path does not contain at least one file path body.
export
readAbsFile : (curdir : Path Abs) -> String -> Either PackErr (File Abs)
readAbsFile cd s = case split $ toAbsPath cd (fromString s) of
  Just (p,b) => Right $ MkF p b
  Nothing    => Left (NoFilePath s)

export
readLogLevel : String -> Either PackErr LogLevel
readLogLevel str = maybeToEither (InvalidLogLevel str) $ lookup str logLevels
