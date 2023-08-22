module Pack.Core.Git

import Data.String
import public Pack.Core.Git.Consts
import Pack.Core.IO
import Pack.Core.Types

%default total

||| (Temporary) Directory to use for a Git project.
export %inline
gitTmpDir : TmpDir => (pkg : PkgName) -> Path Abs
gitTmpDir pkg = tmpDir <//> pkg

||| Cached directory to use for a Git project.
export %inline
gitCacheDir : PackDir => (url : URL) -> Path Abs
gitCacheDir url = packDir <//> ".cache/git" <//> url

parameters {auto has : HasIO io}

  ||| Clones a Git repository to the given destination
  cloneRemote : (url : URL) -> (dest : Path Abs) -> EitherT PackErr io ()
  cloneRemote url dest = sys ["git", "clone", "--depth", "1", "-q", url, dest]

  ||| Creates a shared clone of a cached local git clone
  cloneShared :
       {auto _ : PackDir}
    -> {auto _ : TmpDir}
    -> (url    : URL)
    -> PkgName
    -> EitherT PackErr io ()
  cloneShared url pkg =
    let cache := gitCacheDir url
        tmp   := gitTmpDir pkg
     in sys ["git", "clone", "--shared", "-q", cache, tmp]

  ||| Fetch the given commit from upstream
  fetch : (commit : Commit) -> EitherT PackErr io ()
  fetch commit = sys ["git", "fetch", "-q", "origin", commit]

  ||| Checkout to the given commit
  export
  checkout : (commit : Commit) -> EitherT PackErr io ()
  checkout commit = sys ["git", "checkout", "-q", commit]

  ||| Query a Git repo for the latest commit of the main branch.
  export covering
  gitLatest : (url : URL) -> (branch : Branch) -> EitherT PackErr io Commit
  gitLatest url b =
    MkCommit . fst . break isSpace <$> sysRun ["git", "ls-remote", url, b]

  ||| Clone a git repository into `dir`, switch to the
  ||| given commit and run the given action.
  export
  withGit :
       {auto _ : TmpDir}
    -> {auto _ : PackDir}
    -> (pkg    : PkgName)
    -> (url    : URL)
    -> (commit : Commit)
    -> (act    : Path Abs -> EitherT PackErr io a)
    -> EitherT PackErr io a
  withGit pkg url commit act =
    let cache := gitCacheDir url
        tmp   := gitTmpDir pkg

     in do
       False <- exists tmp | True => inDir tmp act

       -- clone a Git repo if it's not already cached
       when !(missing cache) $ do
         mkParentDir cache
         cloneRemote url cache

       -- fetch the required commit
       inDir cache $ \_ => fetch commit

       mkParentDir tmp

       cloneShared url pkg
       inDir tmp (\d => fetch commit >> checkout commit >> act d)
