module Pack.Types

import Core.FC
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

printErr (UnknownPkg name) = "Unknown package: \{name}"

printErr (NoApp name) = "Package \{name} is not an application"

printErr (LexFail fc err) = show fc ++ ":Lexer error (" ++ show err ++ ")"
printErr (ParseFail errs) = "Parse errors (" ++ show errs ++ ")"
printErr (MissingCorePackage nm v c) =
  "Core package \"\{nm}\" is missing for Idris2 version \{v} (commit: \{c})"

--------------------------------------------------------------------------------
--          Example Database
--------------------------------------------------------------------------------

||| Example package data base
export
db : DB
db = MkDB
  "81ba322a4bb8b4c3bdbc0d42c2f1c2be89a81b23"
  "0.5.1"
  [ MkPackage "elab-util"
              "https://github.com/stefan-hoeck/idris2-elab-util"
              "29f8153c0c06a69a168f009823a31bae266c5306"
              "elab-util.ipkg"

  , MkPackage "sop"
              "https://github.com/stefan-hoeck/idris2-sop"
              "af9224510f5c283f3b3c8293524e51c225617658"
              "sop.ipkg"

  , MkPackage "pretty-show"
              "https://github.com/stefan-hoeck/idris2-pretty-show"
              "d31e1f5a191a78deea1170beee21401da063ed8d"
              "pretty-show.ipkg"

  , MkPackage "hedgehog"
              "https://github.com/stefan-hoeck/idris2-hedgehog"
              "e9858609ea79f0d2e36e66bfb9a2dbceba7771cc"
              "hedgehog.ipkg"

  , MkPackage "pack"
              "https://github.com/stefan-hoeck/idris2-pack"
              "430b7daa3523c570770a24581ddbfd0843d0cca5"
              "pack.ipkg"

  , MkPackage "katla"
              "https://github.com/idris-community/katla"
              "e16c1ec10e9be17cba6f70e91aa3983fa85d521c"
              "katla.ipkg"

  , MkPackage "collie"
              "https://github.com/ohad/collie"
              "46bff04a8d9a1598fec9b19f515541df16dc64ef"
              "collie.ipkg"

  , MkPackage "idrall"
              "https://github.com/alexhumphreys/idrall"
              "b5f04575c94cc5cc006791d81f106f5492e3b8f3"
              "idrall.ipkg" ]
