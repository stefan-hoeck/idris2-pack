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
  CheckDB  : DBName -> File Abs -> ACmd
  FromHEAD : (out : File Abs) -> ACmd
  Help     : ACmd

Command ACmd where
  defaultLevel (CheckDB x y)  = Info
  defaultLevel (FromHEAD out) = Info
  defaultLevel Help           = Warning

  readCommand_ _   ["help"]                 = Right Help
  readCommand_ dir ["extract-from-head",p]  = FromHEAD <$> readAbsFile curDir p
  readCommand_ dir ["check-collection",n,p] =
    [| CheckDB (readDBName n) (readAbsFile curDir p) |]
  readCommand_ _   xs                       = Left $ UnknownCommand xs

  adjConfig (CheckDB db _) c = pure $ {
      collection   := db
    , withDocs     := True
    , useKatla     := True
    , safetyPrompt := False
    } c

  adjConfig (FromHEAD p) c = pure $ {collection := MkDBName "HEAD"} c
  adjConfig Help         c = pure c

  defaultCommand_ = Help


covering
commitOf : HasIO io => Package -> EitherT PackErr io Package
commitOf (GitHub url branch ipkg pp) = do
  commit <- gitLatest url (MkBranch branch.value)
  pure $ GitHub url commit ipkg pp
commitOf p                        = pure p

-- Converts a data base with a branch name for each
-- package to one holding the latest commit hash for each.
covering
dbOf : HasIO io => DB -> EitherT PackErr io DB
dbOf (MkDB u commit v ps) = do
  nc  <- gitLatest u "main"
  nps <- traverse commitOf ps
  pure $ MkDB u nc v nps

-- Converts a data base with a branch name for each
-- package to one holding the latest commit hash for each
-- and writes the resulting DB to the given file.
covering
writeLatestDB : HasIO io => File Abs -> Env -> EitherT PackErr io ()
writeLatestDB path e = do
  ndb <- dbOf e.db
  write path (printDB ndb)

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
    CheckDB db p       => finally (rmDir tmpDir) $ idrisEnv mc True >>= checkDB p
    FromHEAD p         => env mc True >>= writeLatestDB p
    Help               => putStrLn """
      Usage: pack-admin [cmd] [args]

      Commands:

        extract-from-head <path>
          Extract the latest commit hash for all packages in
          the HEAD collection and store the resulting package collection
          at the given path.

        check-collection <colname> <path>
          Tries to build and install all packages from the given package
          collection and prints a basic report.
          Writes the results to the given file.

        help
          Print this help text.
      """
