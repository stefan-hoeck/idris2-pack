module Pack.Core.Git

import Data.String
import public Pack.Core.Git.Consts
import Pack.Core.IO
import Pack.Core.Types

%default total

||| Clones a GitHub repository to the given destination
export
gitClone :  HasIO io
         => (url  : URL)
         -> (dest : Path Abs)
         -> EitherT PackErr io ()
gitClone url dest = sys ["git", "clone", "-q", url, dest]

||| Checkout to the given commit
export
gitCheckout : HasIO io => (commit : Commit) -> EitherT PackErr io ()
gitCheckout commit = sys ["git", "checkout", "-q", commit]

||| Query GitHub for the latest commit of the main branch.
export covering
gitLatest :  HasIO io
          => (url    : URL)
          -> (branch : Branch)
          -> EitherT PackErr io Commit
gitLatest url b =
  MkCommit . fst . break isSpace <$> sysRun ["git", "ls-remote", url, b]

||| (Temporary) Directory to use for a Git project.
export
gitDir : (dir : Path Abs) -> (pkg : PkgName) -> (commit : Commit) -> Path Abs
gitDir dir pkg commit = dir <//> pkg <//> commit

||| Clone a git repository into `dir`, switch to the
||| given commit and run the given action.
export
withGit :  HasIO io
        => (dir    : Path Abs)
        -> (pkg    : PkgName)
        -> (url    : URL)
        -> (commit : Commit)
        -> (act    : Path Abs -> EitherT PackErr io a)
        -> EitherT PackErr io a
withGit p pkg url commit act =
  let dir := gitDir p pkg commit
   in do
     False <- exists dir | True => inDir dir act
     mkParentDir dir
     gitClone url dir
     inDir dir (\d => gitCheckout commit >> act d)
