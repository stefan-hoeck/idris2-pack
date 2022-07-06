module Pack.CmdLn.Types

import Pack.Core.Types
import Pack.Database.Types

%default total

public export
data QueryMode = PkgName | Dependency | Module

||| Commands accepted by *pack*. Most of these
||| operate on a list of packages and/or
||| projects with an `.ipkg` file.
public export
data Cmd : Type where
  -- Developing Idris libs and apps
  Build            : File Abs -> Cmd
  BuildDeps        : File Abs -> Cmd
  Typecheck        : File Abs -> Cmd
  Repl             : Maybe (File Abs) -> Cmd
  Exec             : File Abs -> List String -> Cmd

  -- Package management
  Install          : List (PkgType,PkgName) -> Cmd
  Remove           : List (PkgType,PkgName) -> Cmd
  Run              : Either (File Abs) PkgName -> List String -> Cmd
  New              : (cur : Path Abs) -> PkgType -> Body -> Cmd
  Update           : Cmd

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

export
loglevel : Cmd -> LogLevel
loglevel (Build x)              = Info
loglevel (BuildDeps x)          = Info
loglevel (Typecheck x)          = Info
loglevel (Exec _ _)             = Warning
loglevel (New _ _ _)            = Info
loglevel (Repl x)               = Warning
loglevel (Install xs)           = Info
loglevel (Remove xs)            = Info
loglevel (Run x strs)           = Warning
loglevel Update                 = Info
loglevel PackagePath            = Silence
loglevel LibsPath               = Silence
loglevel DataPath               = Silence
loglevel (AppPath x)            = Silence
loglevel (Switch x)             = Info
loglevel UpdateDB               = Info
loglevel Info                   = Warning
loglevel (Query x str)          = Warning
loglevel (Fuzzy xs str)         = Warning
loglevel (Completion str str1)  = Silence
loglevel (CompletionScript str) = Silence
loglevel PrintHelp              = Silence
