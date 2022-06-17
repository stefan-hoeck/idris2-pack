||| We work a lot with Strings of distinct semantics.
||| Since I've been bitten by this more than once, we wrap
||| the in single field record types to drastically increase
||| type safety.
module Pack.Core.Types

import Data.List1
import Data.String
import Idris.Package.Types
import Libraries.Utils.Path
import System.File

%default total

||| True if the given file path ends on `.ipkg`
export
isIpkgFile : String -> Bool
isIpkgFile = (Just "ipkg" ==) . extension

--------------------------------------------------------------------------------
--          Interpolation
--------------------------------------------------------------------------------

||| Convenience implementation for using paths in string
||| interpolation
export
Interpolation Path where interpolate = show

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

--------------------------------------------------------------------------------
--          DBName
--------------------------------------------------------------------------------

||| Name of a package collection
public export
record DBName where
  constructor MkDBName
  value : String

export %inline
Eq DBName where (==) = (==) `on` value

export %inline
Ord DBName where compare = compare `on` value

export %inline
FromString DBName where fromString = MkDBName

export %inline
Interpolation DBName where interpolate = value

--------------------------------------------------------------------------------
--          Errors
--------------------------------------------------------------------------------

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

export
prefixKey : String -> Either TOMLErr a -> Either TOMLErr a
prefixKey k = mapFst $ \case MissingKey p => MissingKey (k :: p)
                             WrongType p t => WrongType (k :: p) t

||| Errors that can occur when running *pack* programs.
public export
data PackErr : Type where
  ||| Failed to get the path of the current directory.
  CurDir     : PackErr

  ||| Failed to get package directory path
  NoPackDir  : PackErr

  ||| Failed to create the given directory
  MkDir      : (path : Path) -> (err : FileError) -> PackErr

  ||| Failed to read the given file
  ReadFile   : (path : Path) -> (err : FileError) -> PackErr

  ||| Failed to write to the given file
  WriteFile  : (path : Path) -> (err : FileError) -> PackErr

  ||| Failed to read the content of a directory
  DirEntries : (path : Path) -> (err : FileError) -> PackErr

  ||| Error when running the given system command
  Sys        : (cmd : String) -> (err : Int) -> PackErr

  ||| Error when changing into the given directory
  ChangeDir  : (path : Path) -> PackErr

  ||| The given package is not in the package data base
  UnknownPkg : (name : PkgName) -> PackErr

  ||| The given package is not an applicatio
  ||| (No executable name set in the `.ipkg` file)
  NoApp      : (rep : PkgName) -> PackErr

  ||| The given package is not an applicatio
  ||| (No executable name set in the `.ipkg` file)
  NoAppIpkg  : (path : Path) -> PackErr

  ||| An erroneous package description in a package DB file
  InvalidPackageDesc : (s : String) -> PackErr

  ||| The package database is empty (no header)
  EmptyPkgDB : PackErr

  ||| Invalid package database header
  InvalidDBHeader : (s : String) -> PackErr

  ||| Invalid package version
  InvalidPkgVersion : (s : String) -> PackErr

  ||| Failed to parse an .ipkg file
  InvalidIpkgFile  : (path : Path) -> PackErr

  ||| The given core package (base, contrib, etc.)
  ||| is missing from the Idris installation.
  MissingCorePackage :  (name    : PkgName)
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
  BuildMany : PackErr

  ||| Unknown command or sequence of options
  ||| entered on the command line
  UnknownCommand : List String -> PackErr

  ||| Trying to clone a repository into an existing
  ||| directory.
  DirExists : Path -> PackErr

  ||| Error in a toml file.
  TOMLFile :  (file : Path) -> (err : TOMLErr) -> PackErr

  ||| Error in a toml file.
  TOMLParse : (file : Path) -> (err : String) -> PackErr

  ||| Number of failures when building packages.
  BuildFailures : Nat -> PackErr

||| Prints an error that occured during program execution.
export
printErr : PackErr -> String
printErr CurDir = "Failed to get current directory."

printErr NoPackDir = """
  Failed to figure out package directory.
  This means, that neither environment variable \"PACK_DIR\"
  nor environment varaible \"HOME\" was set.
  """

printErr (MkDir path err) =
  "Error when creating directory \"\{path}\": \{err}."

printErr (ReadFile path err) =
  "Error when reading file \"\{path}\": \{err}."

printErr (WriteFile path err) =
  "Error when writing to file \"\{path}\": \{err}."

printErr (DirEntries path err) =
  "Error when reading directory \"\{path}\": \{err}."

printErr (Sys cmd err) = """
  Error when executing system command.
  Command: \{cmd}
  Error code: \{show err}
  """

printErr (ChangeDir path) =
  "Failed to change to directory \"\{path}\"."

printErr (InvalidPackageDesc s) = """
  Invalid package description: \"\{s}\".
  This should be of the format \"name,url,commit hash,ipkg file\".
  """

printErr (InvalidDBHeader s) = """
  Invalid data base header: \"\{s}\".
  This should be of the format \"idris2 commit hash,idris2 version\".
  """

printErr (InvalidPkgVersion s) = "Invalid package version: \"\{s}\"."

printErr (UnknownPkg name) = "Unknown package: \{name}"

printErr (NoApp rep) = "Package \{rep} is not an application"

printErr (NoAppIpkg p) = "Package \{p} is not an application"

printErr EmptyPkgDB = "Empty package data base"

printErr (InvalidIpkgFile path) =
  "Failed to parse .ipkg file: \{path}"

printErr (MissingCorePackage nm v c) =
  "Core package \"\{nm}\" missing for Idris2 version \{v} (commit: \{c})"

printErr (UnknownArg arg) = "Unknown command line arg: \{arg}"

printErr (InvalidArgs args) = "Invalid command line args: \{unwords args}"

printErr (ErroneousArg err) = err

printErr (UnknownCommand cmd) = "Unknown command: \{unwords cmd}"

printErr BuildMany =
  "Can only build or typecheck a single Idris2 package given as an `.ipkg` file."

printErr (DirExists path) = """
  Failed to clone GitHub repository into \{path}.
  Directory already exists.
  """

printErr (TOMLFile file err) =
  "Error in file \{file}: \{printTOMLErr err}."

printErr (TOMLParse file err) = "Error in file \{file}: \{err}."

printErr (BuildFailures 1) = "1 package failed to build."

printErr (BuildFailures n) = "\{show n} packages failed to build."

--------------------------------------------------------------------------------
--          Logging
--------------------------------------------------------------------------------

public export
data LogLevel = Debug | Info | Warning

llToNat : LogLevel -> Nat
llToNat Debug   = 0
llToNat Info    = 1
llToNat Warning = 2

export
Eq LogLevel where (==) = (==) `on` llToNat

export
Ord LogLevel where compare = compare `on` llToNat

export
Interpolation LogLevel where
  interpolate Debug   = "debug"
  interpolate Info    = "info"
  interpolate Warning = "warning"
