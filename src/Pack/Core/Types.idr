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
Show URL where show = value

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
Show Commit where show = value

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
Show PkgName where show = value

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
Show DBName where show = value

export %inline
FromString DBName where fromString = MkDBName

export %inline
Interpolation DBName where interpolate = value

--------------------------------------------------------------------------------
--          PkgRep
--------------------------------------------------------------------------------

||| Package representation at the command line.
||| This is either a path to an `.ipkg` file or the
||| name of a package in the package collection.
public export
data PkgRep : Type where
  Pkg  : PkgName -> PkgRep
  Ipkg : Path    -> PkgRep

export
Interpolation PkgRep where
  interpolate (Pkg n)  = n.value
  interpolate (Ipkg p) = show p

export
isIpkgFile : String -> Bool
isIpkgFile = (Just "ipkg" ==) . extension

export
FromString PkgRep where
  fromString s = if isIpkgFile s then Ipkg (parse s) else Pkg (MkPkgName s)

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
  NoApp      : (rep : PkgRep) -> PackErr

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
  "Error when creating directory \"\{show path}\": \{show err}."

printErr (ReadFile path err) =
  "Error when reading file \"\{show path}\": \{show err}."

printErr (WriteFile path err) =
  "Error when writing to file \"\{show path}\": \{show err}."

printErr (DirEntries path err) =
  "Error when reading directory \"\{show path}\": \{show err}."

printErr (Sys cmd err) = """
  Error when executing system command.
  Command: \{cmd}
  Error code: \{show err}
  """

printErr (ChangeDir path) =
  "Failed to change to directory \"\{show path}\"."

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

printErr EmptyPkgDB = "Empty package data base"

printErr (InvalidIpkgFile path) =
  "Failed to parse .ipkg file: \{show path}"

printErr (MissingCorePackage nm v c) =
  "Core package \"\{nm}\" missing for Idris2 version \{show v} (commit: \{c})"

printErr (UnknownArg arg) = "Unknown command line arg: \{arg}"

printErr (InvalidArgs args) = "Invalid command line args: \{unwords args}"

printErr (ErroneousArg err) = err

printErr (UnknownCommand cmd) = "Unknown command: \{unwords cmd}"

printErr BuildMany =
  "Can only build or typecheck a single Idris2 package given as an `.ipkg` file."

printErr (DirExists path) = """
  Failed to clone GitHub repository into \{show path}.
  Directory already exists.
  """

printErr (TOMLFile file err) =
  "Error in file \{show file}: \{printTOMLErr err}."

printErr (TOMLParse file err) = "Error in file \{show file}: \{err}."

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
Show LogLevel where
  show Debug   = "debug"
  show Info    = "info"
  show Warning = "warning"
