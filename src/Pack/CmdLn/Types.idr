module Pack.CmdLn.Types

import Pack.Core.Types
import Pack.Database.Types

%default total

||| Mode used for querying the package collection:
||| By package name, by dependency, or by module name.
public export
data QueryMode = PkgName | Dependency | Module

||| Commands accepted by *pack*. Most of these
||| operate on a list of packages and/or
||| projects with an `.ipkg` file.
public export
data Cmd : Type where
  -- Developing Idris libs and apps
  Build            : (ipkg : File Abs) -> Cmd
  BuildDeps        : (ipkg : File Abs) -> Cmd
  Typecheck        : (ipkg : File Abs) -> Cmd
  Repl             : (src : Maybe $ File Abs) -> Cmd
  Exec             : (srd : File Abs) -> (args : List String) -> Cmd

  -- Package management
  Install          : List (PkgType,PkgName) -> Cmd
  Remove           : List (PkgType,PkgName) -> Cmd
  Run              : Either (File Abs) PkgName -> List String -> Cmd
  New              : (cur : CurDir) -> PkgType -> Body -> Cmd
  Update           : Cmd
  Fetch            : Cmd

  -- Idris environment
  PackagePath      : Cmd
  LibsPath         : Cmd
  DataPath         : Cmd
  AppPath          : PkgName -> Cmd

  -- Managing package collections
  Switch           : DBName -> Cmd
  UpdateDB         : Cmd

  -- Queries
  Info             : Cmd
  Query            : QueryMode -> String -> Cmd
  Fuzzy            : List PkgName -> String -> Cmd

  -- Tab completion
  Completion       : String -> String -> Cmd
  CompletionScript : String -> Cmd

  -- Help
  PrintHelp        : Cmd
