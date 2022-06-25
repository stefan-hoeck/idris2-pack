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
  Build            : AbsFile -> Cmd
  BuildDeps        : AbsFile -> Cmd
  Typecheck        : AbsFile -> Cmd
  Repl             : Maybe AbsFile -> Cmd

  -- Package management
  Install          : List PkgName -> Cmd
  InstallApp       : List PkgName -> Cmd
  Remove           : List PkgName -> Cmd
  Run              : Either AbsFile PkgName -> List String -> Cmd
  New              : PkgType -> String -> Cmd

  -- Idris environment
  PackagePath      : Cmd
  LibsPath         : Cmd
  DataPath         : Cmd

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
loglevel (New _ _)              = Info
loglevel (Repl x)               = Warning
loglevel (Install xs)           = Info
loglevel (InstallApp xs)        = Info
loglevel (Remove xs)            = Info
loglevel (Run x strs)           = Warning
loglevel PackagePath            = Warning
loglevel LibsPath               = Warning
loglevel DataPath               = Warning
loglevel (Switch x)             = Info
loglevel UpdateDB               = Info
loglevel Info                   = Warning
loglevel (Query x str)          = Warning
loglevel (Fuzzy xs str)         = Warning
loglevel (Completion str str1)  = Warning
loglevel (CompletionScript str) = Warning
loglevel PrintHelp              = Warning
