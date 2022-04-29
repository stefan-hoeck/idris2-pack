module Pack.Err

import Core.FC
import System.File

||| Errors that can occur when running *pack* programs.
public export
data PackErr : Type where
  ||| Failed to get the path of the current directory.
  CurDir     : PackErr

  ||| Failed to get package directory path
  NoPackDir  : PackErr

  ||| Failed to create the given directory
  MkDir      : (path : String) -> (err : FileError) -> PackErr

  ||| Failed to read the given file
  ReadFile   : (path : String) -> (err : FileError) -> PackErr

  ||| Failed to write to the given file
  WriteFile  : (path : String) -> (err : FileError) -> PackErr

  ||| Error when running the given system command
  Sys        : (cmd : String) -> (err : Int) -> PackErr

  ||| Error when changing into the given directory
  ChangeDir  : (path : String) -> PackErr

  ||| The given package is not in the package data base
  UnknownPkg : (name : String) -> PackErr

  ||| The given package is not an applicatio
  ||| (No executable name set in the `.ipkg` file)
  NoApp      : (name : String) -> PackErr

  ||| An erroneous package description in a package DB file
  InvalidPackageDesc : (s : String) -> PackErr

  ||| The package database is empty (no header)
  EmptyPkgDB : PackErr

  ||| Invalid package database header
  InvalidDBHeader : (s : String) -> PackErr

  ||| Failed to lex the given .ipkg file
  LexFail    : FC -> String -> PackErr

  ||| Failed to parse an .ipkg file
  ParseFail  : List (FC, String) -> PackErr

  ||| The given core package (base, contrib, etc.)
  ||| is missing from the Idris installation.
  MissingCorePackage :  (name : String)
                     -> (version : String)
                     -> (commit : String)
                     -> PackErr

  ||| Unknown command line argument
  UnknownArg : (arg : String) -> PackErr

  ||| Erroneous command line argument
  ErroneousArg : (err : String) -> PackErr

  ||| No repository was specified
  MissingRepo : PackErr

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
  "Error when creating directory \"\{path}\": \{show err}."

printErr (ReadFile path err) =
  "Error when reading file \"\{path}\": \{show err}."

printErr (WriteFile path err) =
  "Error when writing to file \"\{path}\": \{show err}."

printErr (Sys cmd err) = """
  Error when executing system command.
  Command: \{cmd}
  Error code: \{show err}
  """

printErr (ChangeDir path) = "Failed to change to directory \"\{path}\"."

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

printErr (LexFail fc err) = show fc ++ ":Lexer error (" ++ show err ++ ")"

printErr (ParseFail errs) = "Parse errors (" ++ show errs ++ ")"

printErr (MissingCorePackage nm v c) =
  "Core package \"\{nm}\" is missing for Idris2 version \{v} (commit: \{c})"

printErr (UnknownArg arg) = "Unknown command line arg: \{arg}"

printErr (ErroneousArg err) = err

printErr MissingRepo = "Please specify a single repository to switch to."
