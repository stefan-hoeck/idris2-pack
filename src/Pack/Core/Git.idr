module Pack.Core.Git

import Data.String
import Pack.Core.IO
import Pack.Core.Types

%default total

||| URL of package collections repository
|||
||| TODO: This should become a command line argument
export
dbRepo : URL
dbRepo = "https://github.com/stefan-hoeck/idris2-pack-db"

||| Clones a GitHub repository to the given destination
export
gitClone :  HasIO io
         => (url  : URL)
         -> (dest : Path)
         -> EitherT PackErr io ()
gitClone url dest = sys "git clone -q \{url} \{dest}"

||| Checkout to the given commit
export
gitCheckout : HasIO io => (commit : Commit) -> EitherT PackErr io ()
gitCheckout commit = sys "git checkout -q \{commit}"

||| Query GitHub for the latest commit of the main branch.
export covering
gitLatest :  HasIO io
          => (url    : URL)
          -> (commit : Commit)
          -> EitherT PackErr io Commit
gitLatest url c =
  MkCommit . fst . break isSpace <$> sysRun "git ls-remote \{url} \{c}"

||| Clone a git repository into `dir`, switch to the
||| given commit and run the given action.
|||
||| CAUTION: `dir` must be empty or non-existant, otherwise
|||          this will fail.
export
withGit :  HasIO io
        => (dir    : Path)
        -> (url    : URL)
        -> (commit : Commit)
        -> (act    : EitherT PackErr io a)
        -> EitherT PackErr io a
withGit dir url commit act = do
  False <- exists dir | True => throwE (DirExists dir)
  finally (rmDir dir) $ do
    gitClone url dir
    inDir dir (gitCheckout commit >> act)
