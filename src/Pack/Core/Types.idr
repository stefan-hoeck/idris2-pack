module Pack.Core.Types

import Data.String
import Libraries.Utils.Path
import System.File

%default total

--------------------------------------------------------------------------------
--          Commits
--------------------------------------------------------------------------------

||| A git commit
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

||| A git commit
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

||| A git commit
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
--          Errors
--------------------------------------------------------------------------------

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

  ||| Error when running the given system command
  Sys        : (cmd : String) -> (err : Int) -> PackErr

  ||| Error when changing into the given directory
  ChangeDir  : (path : Path) -> PackErr

  ||| The given package is not in the package data base
  UnknownPkg : (name : PkgName) -> PackErr

  ||| The given package is not an applicatio
  ||| (No executable name set in the `.ipkg` file)
  NoApp      : (name : PkgName) -> PackErr

  ||| An erroneous package description in a package DB file
  InvalidPackageDesc : (s : String) -> PackErr

  ||| The package database is empty (no header)
  EmptyPkgDB : PackErr

  ||| Invalid package database header
  InvalidDBHeader : (s : String) -> PackErr

  ||| Failed to parse an .ipkg file
  ParseFail  : (path : Path) -> PackErr

  ||| The given core package (base, contrib, etc.)
  ||| is missing from the Idris installation.
  MissingCorePackage :  (name : PkgName)
                     -> (version : String)
                     -> (commit : Commit)
                     -> PackErr

  ||| Unknown command line argument
  UnknownArg : (arg : String) -> PackErr

  ||| Erroneous command line argument
  ErroneousArg : (err : String) -> PackErr

  ||| Trying to run zero or more than one local package
  ||| (or something that isn't a local package).
  BuildMany : PackErr

  ||| Unknown command or sequence of options
  ||| entered on the command line
  UnknownCommand : List String -> PackErr

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

printErr (UnknownPkg name) = "Unknown package: \{name}"

printErr (NoApp name) = "Package \{name} is not an application"

printErr EmptyPkgDB = "Empty package data base"

printErr (ParseFail path) =
  "Failed to parse .ipkg file: \{show path}"

printErr (MissingCorePackage nm v c) =
  "Core package \"\{nm}\" is missing for Idris2 version \{v} (commit: \{c})"

printErr (UnknownArg arg) = "Unknown command line arg: \{arg}"

printErr (ErroneousArg err) = err

printErr (UnknownCommand cmd) = "Unknown command: \{unwords cmd}"

printErr BuildMany =
  "Can only build or typecheck a single Idris2 package given as an `.ipkg` file."
