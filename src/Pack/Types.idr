module Pack.Types

import Core.FC
import Data.List1
import Data.String
import Idris.Package.Types
import System.File

%default total

--------------------------------------------------------------------------------
--          Packages
--------------------------------------------------------------------------------

||| Description of an Idris package on GitHub
|||
||| Eventually, I'd like to list these in a data base
||| (probably just a clear-text file) of curated Idris
||| packages together with an Idris version to use for them.
public export
record Package where
  constructor MkPackage
  ||| Name of the package. This should be the
  ||| same as the one given in the .ipkg file
  ||| given below.
  name   : String

  ||| Link to the github page
  url    : String

  ||| Version (git commit) to use
  commit : String

  ||| Name of ".ipkg" file
  ipkg   : String

||| A resolved package, which was downloaded from GitHub
||| and the version and dependencies of which where extracted
||| from its ".ipkg" file.
|||
||| Eventually, I'd like to generate a DB (probably just a
||| text file) of resolved packages from a list of packages.
public export
data ResolvedPackage : Type where
  RP :  (pkg  : Package)
     -> (desc : PkgDesc)
     -> ResolvedPackage

  Base    : ResolvedPackage
  Contrib : ResolvedPackage
  Idris2  : ResolvedPackage
  Linear  : ResolvedPackage
  Network : ResolvedPackage
  Prelude : ResolvedPackage
  Test    : ResolvedPackage

export
isCorePackage : ResolvedPackage -> Bool
isCorePackage (RP _ _) = False
isCorePackage Base     = True
isCorePackage Contrib  = True
isCorePackage Linear   = True
isCorePackage Idris2   = True
isCorePackage Network  = True
isCorePackage Prelude  = True
isCorePackage Test     = True

--------------------------------------------------------------------------------
--          Package Database
--------------------------------------------------------------------------------

||| DB used for building packages. This includes
||| the Idris commit to use, together with a curated list of
||| known packages.
public export
record DB where
  constructor MkDB
  idrisCommit  : String
  idrisVersion : String
  packages     : List Package

--------------------------------------------------------------------------------
--          Build Environment
--------------------------------------------------------------------------------

||| Environment used for building packages.
public export
record Env where
  constructor MkEnv
  db           : DB
  packDir      : String
  packTmpDir   : String
  packIdrisDir : String

--------------------------------------------------------------------------------
--          Errors
--------------------------------------------------------------------------------

||| Errors that can occur when running *pack* programs.
public export
data PackErr : Type where
  CurDir     : PackErr
  NoPackDir  : PackErr
  MkDir      : (path : String) -> (err : FileError) -> PackErr
  ReadFile   : (path : String) -> (err : FileError) -> PackErr
  Sys        : (cmd : String) -> (err : Int) -> PackErr
  ChangeDir  : (path : String) -> PackErr
  UnknownPkg : (name : String) -> PackErr
  NoApp      : (name : String) -> PackErr
  InvalidPackageDesc : (s : String) -> PackErr
  EmptyPkgDB : PackErr
  InvalidDBHeader : (s : String) -> PackErr
  LexFail    : FC -> String -> PackErr
  ParseFail  : List (FC, String) -> PackErr
  MissingCorePackage :  (name : String)
                     -> (version : String)
                     -> (commit : String)
                     -> PackErr

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

--------------------------------------------------------------------------------
--          Reading a Database
--------------------------------------------------------------------------------

commaSep : String -> List String
commaSep = forget . split (',' ==)

readPkg : String -> Either PackErr Package
readPkg s = case commaSep s of
  [n,url,hash,pkg] => Right $ MkPackage n url hash pkg
  _                => Left (InvalidPackageDesc s)

export
readDB : String -> Either PackErr DB
readDB s = case lines s of
  []       => Left EmptyPkgDB
  (h :: t) => case commaSep h of
    [c,v] => MkDB c v <$> traverse readPkg t
    _     => Left (InvalidDBHeader h)
