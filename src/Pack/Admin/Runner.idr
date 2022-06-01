module Pack.Admin.Runner

import Data.SortedMap
import Libraries.Utils.Path
import Pack.Admin.Runner.Check
import Pack.CmdLn.Opts
import Pack.CmdLn.Types
import Pack.Config.Env
import Pack.Config.Types
import Pack.Database.Types
import Pack.Runner.Install
import public Pack.Core

data ACmd : Type where
  CheckDB  : DBName -> ACmd
  FromHEAD : (path : Path) -> ACmd
  Help     : ACmd

readCmd : List String -> Either PackErr ACmd
readCmd ["help"]                = Right Help
readCmd ["extract-from-head",p] = Right (FromHEAD $ parse p)
readCmd ["check-collection",n]  = Right (CheckDB $ MkDBName n)
readCmd xs                      = Left $ UnknownCommand xs

covering
commitOf : HasIO io => Package -> EitherT PackErr io Package
commitOf (GitHub url branch ipkg) = do
  commit <- gitLatest url branch
  pure $ GitHub url commit ipkg
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
writeLatestDB : HasIO io => Path -> Env s -> EitherT PackErr io ()
writeLatestDB path e = do
  ndb <- dbOf e.db
  write path (printDB ndb)

setColl : DBName -> Config e -> Config e
setColl db = {collection := db}

export covering
runCmd : HasIO io => EitherT PackErr io ()
runCmd = do
  (c',cmd) <- getConfig readCmd Help
  case cmd of
    CheckDB db         =>
      let c = setColl db c'
       in idrisEnv c >>= checkDB
    FromHEAD p         =>
      let c = setColl "HEAD" c'
       in env c >>= writeLatestDB p
    Help               => putStrLn """
      Usage: pack-admin [cmd] [args]

      Commands:

        extract-from-head <path>
          Extract the latest commit hash for all packages in
          the HEAD collection and store the resulting package collection
          at the given path.

        check-collection <colname>
          Tries to build and install all packages from the given package
          collection and prints a basic report.

        help
          Print this help text.
      """
