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
  Help     : ACmd

commands : List ACmd
commands = [CheckDB, FromHEAD, Help]

name : ACmd -> String
name CheckDB  = "check-collection"
name FromHEAD = "extract-from-head"
name Help     = "help"

namesAndCommands : List (String,ACmd)
namesAndCommands = map (\c => (name c, c)) commands

cmdDesc : ACmd -> String
cmdDesc CheckDB  = """
  Tries to build and install all packages from the given package
  collection and prints a basic report.
  Writes the results to the given file.
  """

cmdDesc FromHEAD = """
  Extract the latest commit hash for all packages in
  the HEAD collection and store the resulting package collection
  at the given path.
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
  defaultLevel Help     = Warning

  ArgTypes CheckDB  = [DBName, File Abs]
  ArgTypes FromHEAD = [File Abs]
  ArgTypes Help     = [Maybe ACmd]

  readCommand_ n = lookup n namesAndCommands

  adjConfig_ CheckDB [db,_] c = pure $ {
      collection   := db
    , withDocs     := False
    , useKatla     := False
    , safetyPrompt := False
    } c

  adjConfig_ FromHEAD [p] c = pure $ {collection := MkDBName "HEAD"} c
  adjConfig_ Help     [_] c = pure c

  desc = cmdDesc

  readArgs CheckDB  = %search
  readArgs FromHEAD = %search
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
  td       <- mkTmpDir
  cd       <- CD <$> curDir
  cache    <- emptyCache
  (mc,cmd) <- getConfig ACmd
  linebuf  <- getLineBufferingCmd
  case cmd of
    (CheckDB ** [db,p]) => finally (rmDir tmpDir) $ idrisEnv mc True >>= checkDB p
    (FromHEAD ** [p])   => env mc True >>= writeLatestDB p
    (Help ** [c])       => putStrLn (usageDesc c)
