module Pack.Admin.Runner

import Data.IORef
import Data.SortedMap
import Pack.Admin.Runner.Check
import Pack.CmdLn.Opts
import Pack.CmdLn.Types
import Pack.Config
import Pack.Database
import Pack.Runner.Install
import public Pack.Core

data ACmd : Type where
  CheckDB  : ACmd
  FromHEAD : ACmd
  MakeDocs : ACmd
  Help     : ACmd

commands : List ACmd
commands = [CheckDB, FromHEAD, MakeDocs, Help]

name : ACmd -> String
name CheckDB  = "check-collection"
name FromHEAD = "extract-from-head"
name MakeDocs = "api-docs"
name Help     = "help"

namesAndCommands : List (String,ACmd)
namesAndCommands = map (\c => (name c, c)) commands

cmdDesc : ACmd -> String
cmdDesc CheckDB  = """
  Tries to build and install all packages from the current package
  collection and prints a basic report.
  Writes the results to `Status.md` in the given directory. A copy
  of the tested DB is put there as well if all goes well.

  Note: This does not terminate with an error condition, even if
  some of the checks fail. In that case, the tested collection is
  not copied.
  """

cmdDesc FromHEAD = """
  Extract the latest commit hash for all packages in
  the HEAD collection and store the resulting package collection
  at the given path.
  """

cmdDesc MakeDocs = """
  Generates HTML docs for all packages in the current package collection.
  The generated docs will be linked with semantically highlighted
  sources, generated by katla.
  """

cmdDesc Help     = """
  Without an additional <cmd> argument, this prints general information
  about using pack-admin, including a list of available command-line options
  and a description of what each of them does.

  If an explicit command is given, this gives some detail about what
  the command in question does and what additional arguments it takes.

  Available commands:
  \{unlines $ map (indent 2 . fst) Runner.namesAndCommands}
  """

export
Arg ACmd where
  argDesc_ = "<cmd>"

  readArg = parseSingleMaybe (`lookup` namesAndCommands)

Command ACmd where
  defaultCommand = Help

  appName = "pack-admin"

  usage = """
  Run `pack-admin help <cmd>` to get detailed information about a command.

  Available commands:
  \{unlines $ map (indent 2 . fst) Runner.namesAndCommands}
  """

  cmdName = name

  defaultLevel CheckDB  = Build
  defaultLevel FromHEAD = Info
  defaultLevel MakeDocs = Build
  defaultLevel Help     = Warning

  ArgTypes CheckDB  = [Path Abs]
  ArgTypes MakeDocs = []
  ArgTypes FromHEAD = [File Abs]
  ArgTypes Help     = [Maybe ACmd]

  readCommand_ n = lookup n namesAndCommands

  adjConfig_ CheckDB [_] c = pure $ {
      withDocs     := False
    , useKatla     := False
    , safetyPrompt := False
    } c

  adjConfig_ MakeDocs [] c = pure $ {
      withDocs     := True
    , useKatla     := True
    , safetyPrompt := False
    } c

  adjConfig_ FromHEAD [p] c = pure $ {collection := MkDBName "HEAD"} c
  adjConfig_ Help     [_] c = pure c

  desc = cmdDesc

  readArgs CheckDB  = %search
  readArgs FromHEAD = %search
  readArgs MakeDocs = %search
  readArgs Help     = %search

-- Converts a data base with a branch name for each
-- package to one holding the latest commit hash for each
-- and writes the resulting DB to the given file.
covering
writeLatestDB : HasIO io => File Abs -> Env -> EitherT PackErr io ()
writeLatestDB path e = write path (printDB e.db)

export covering
runCmd : HasIO io => EitherT PackErr io ()
runCmd = do
  pd       <- getPackDir
  withTmpDir $ do
    cd       <- CD <$> curDir
    cache    <- emptyCache
    (mc,cmd) <- getConfig ACmd
    linebuf  <- getLineBufferingCmd
    case cmd of
      (CheckDB ** [p])  => idrisEnv mc True >>= checkDB p
      (FromHEAD ** [p]) => env mc True >>= writeLatestDB p
      (MakeDocs ** [])  => idrisEnv mc True >>= makeDocs
      (Help ** [c])     => putStrLn (usageDesc c)
