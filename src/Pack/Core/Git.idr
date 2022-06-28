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
         -> (dest : Path Abs)
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

ndir : Nat -> Path Abs -> Path Abs
ndir n p =
  let Just b := Body.parse (show n) | Nothing => p
   in p /> b

withGit' :  HasIO io
         => (p      : Path Abs)
         -> (n      : Nat)
         -> (url    : URL)
         -> (commit : Commit)
         -> (act    : Path Abs -> EitherT PackErr io a)
         -> EitherT PackErr io a
withGit' p n url commit act =
  let dir := maybe p (p <.>) $ Body.parse (show n)
   in do
     False <- exists dir
       | True => case n of
           S k => withGit' p k url commit act
           0   => throwE (DirExists dir)
     finally (rmDir dir) $ do
       gitClone url dir
       inDir dir (\d => gitCheckout commit >> act d)

||| Clone a git repository into `dir`, switch to the
||| given commit and run the given action.
export
withGit :  HasIO io
        => (dir    : Path Abs)
        -> (url    : URL)
        -> (commit : Commit)
        -> (act    : Path Abs -> EitherT PackErr io a)
        -> EitherT PackErr io a
withGit dir = withGit' dir 1000
