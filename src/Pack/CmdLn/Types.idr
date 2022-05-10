module Pack.CmdLn.Types

import Pack.Core.Types
import Pack.Database.Types
import Libraries.Utils.Path

%default total

||| Commands accepted by *pack*. Most of these
||| operate on a list of packages and/or
||| projects with an `.ipkg` file.
public export
data Cmd : Type where
  Build            : Path -> Cmd
  Typecheck        : Path -> Cmd
  Exec             : PkgRep -> List String -> Cmd
  Repl             : Maybe Path -> Cmd

  Install          : List PkgRep -> Cmd
  InstallApp       : List PkgRep -> Cmd
  Remove           : List PkgRep -> Cmd

  CheckDB          : DBName -> Cmd
  FromHEAD         : Path -> Cmd
  UpdateDB         : Cmd

  Query            : String -> Cmd
  Completion       : String -> String -> Cmd
  CompletionScript : String -> Cmd

  PrintHelp        : Cmd
