module Pack.Runner.Database

import Data.SortedMap
import Idris.Package.Types
import Libraries.Utils.Path
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types

%default total

covering
commitOf : HasIO io => Package -> EitherT PackErr io Package
commitOf (GitHub url branch ipkg) = do
  commit <- gitLatest url branch
  pure $ GitHub url commit ipkg
commitOf p                        = pure p

||| Converts a data base with a branch name for each
||| package to one holding the latest commit hash for each.
export covering
dbOf : HasIO io => DB -> EitherT PackErr io DB
dbOf (MkDB commit v ps) = do
  nc  <- gitLatest idrisRepo "main"
  nps <- traverse commitOf ps
  pure $ MkDB nc v nps

||| Converts a data base with a branch name for each
||| package to one holding the latest commit hash for each
||| and writes the resulting DB to the given file.
export covering
writeLatestDB : HasIO io => Path -> Env s -> EitherT PackErr io ()
writeLatestDB path e = do
  ndb <- dbOf e.db
  write path (printDB ndb)

||| Check if a package has already been built and installed
export
packageExists :  HasIO io
              => (env : Env s)
              -> ResolvedPackage
              -> EitherT PackErr io Bool
packageExists env p = exists (packageInstallDir env p)

||| Check if the executable of a package has already been
||| built and installed
export
executableExists : HasIO io => Config s -> String -> EitherT PackErr io Bool
executableExists c p = exists (packageExec c p)

||| Lookup a package name in the package data base,
||| then download and extract its `.ipkg` file from
||| its GitHub repository.
export covering
resolve : HasIO io => (e : Env s) -> PkgRep -> EitherT PackErr io ResolvedPackage
resolve _ (Pkg "base")    = pure Base
resolve _ (Pkg "contrib") = pure Contrib
resolve _ (Pkg "linear")  = pure Linear
resolve _ (Pkg "idris2")  = pure Idris2
resolve _ (Pkg "network") = pure Network
resolve _ (Pkg "prelude") = pure Prelude
resolve _ (Pkg "test")    = pure Test
resolve e (Ipkg path)     = RIpkg path <$> parseIpkgFile path
resolve e (Pkg n)         = case lookup n (allPackages e) of
  Nothing  => throwE (UnknownPkg n)

  -- this is a known package so we download its `.ipkg`
  -- file from GitHub and patch it, if necessary
  Just (GitHub url commit ipkg) =>
    let cache = ipkgPath e n commit ipkg
     in do
       b <- exists cache
       case b of
         True => RGitHub n url commit ipkg <$> parseIpkgFile cache
         False => withGit (tmpDir e) url commit $ do
           let pf = patchFile e n ipkg
           when !(exists pf) (patch ipkg pf)
           copyFile ipkg cache
           RGitHub n url commit ipkg <$> parseIpkgFile ipkg
  Just (Local dir ipkg) =>
    inDir dir $ RLocal n dir ipkg <$> parseIpkgFile ipkg
