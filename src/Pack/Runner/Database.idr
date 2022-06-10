module Pack.Runner.Database

import Data.SortedMap
import Idris.Package.Types
import Libraries.Utils.Path
import Pack.Config.Env
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types

%default total

||| Check if a package has already been built and installed
export
packageExists :  HasIO io
              => (env : Env s)
              -> ResolvedPackage
              -> EitherT PackErr io Bool
packageExists env p =
  let dir = packageInstallDir env p
   in do
       debug env "Looking for package \{name p} at \{dir}"
       exists dir

||| Check if the executable of a package has already been
||| built and installed
export
executableExists :  HasIO io
                 => Env s
                 -> ResolvedPackage
                 -> String
                 -> EitherT PackErr io Bool
executableExists c rp n =
  let pth = packageExec c rp n
   in do
       debug c "Looking for executable \{n} at \{pth}"
       exists pth

export
cacheCoreIpkgFiles : HasIO io => Env s -> EitherT PackErr io ()
cacheCoreIpkgFiles e = do
  copyFile (parse "libs/prelude/prelude.ipkg") (preludePath e)
  copyFile (parse "libs/base/base.ipkg") (basePath e)
  copyFile (parse "libs/contrib/contrib.ipkg") (contribPath e)
  copyFile (parse "libs/network/network.ipkg") (networkPath e)
  copyFile (parse "libs/linear/linear.ipkg") (linearPath e)
  copyFile (parse "libs/test/test.ipkg") (testPath e)
  copyFile (parse "idris2api.ipkg") (idrisApiPath e)

resolveCore :  HasIO io
            => Env s
            -> Path
            -> CorePkg
            -> EitherT PackErr io (String, ResolvedPackage)
resolveCore e pth c = do
  when !(missing pth) $
    withGit (tmpDir e) e.db.idrisURL e.db.idrisCommit (cacheCoreIpkgFiles e)
  parseIpkgFile pth (Core c)

covering
resolveImpl :  HasIO io
            => (e : Env s)
            -> PkgRep
            -> EitherT PackErr io (String, ResolvedPackage)
resolveImpl e (Pkg "base")    = resolveCore e (basePath e) Base
resolveImpl e (Pkg "contrib") = resolveCore e (contribPath e) Contrib
resolveImpl e (Pkg "linear")  = resolveCore e (linearPath e) Linear
resolveImpl e (Pkg "idris2")  = resolveCore e (idrisApiPath e) IdrisApi
resolveImpl e (Pkg "network") = resolveCore e (networkPath e) Network
resolveImpl e (Pkg "prelude") = resolveCore e (preludePath e) Prelude
resolveImpl e (Pkg "test")    = resolveCore e (testPath e) Test
resolveImpl e (Ipkg path)     = parseIpkgFile path (RIpkg path)
resolveImpl e (Pkg n)         = case lookup n (allPackages e) of
  Nothing  => throwE (UnknownPkg n)

  -- this is a known package so we download its `.ipkg`
  -- file from GitHub and patch it, if necessary
  Just (GitHub url commit ipkg pp) =>
    let cache = ipkgPath e n commit ipkg
     in do
       when !(missing cache) $
         withGit (tmpDir e) url commit $ do
           let pf = patchFile e n ipkg
           when !(exists pf) (patch ipkg pf)
           copyFile ipkg cache
       parseIpkgFile cache (RGitHub n url commit ipkg pp)
  Just (Local dir ipkg pp) =>
    inDir dir $ parseIpkgFile ipkg (RLocal n dir ipkg pp)

execStr : PkgDesc -> String
execStr d = maybe "library" (const "application") d.executable

descStr : ResolvedPackage -> String
descStr (RGitHub name url commit ipkg _ desc) =
  "GitHub \{execStr desc} (\{url}:\{commit})"
descStr (RIpkg path desc) = ".ipkg \{execStr desc}"
descStr (RLocal name dir ipkg _ desc) = "local \{execStr desc}"
descStr _ = "core package"

||| Lookup a package name in the package data base,
||| then download and extract its `.ipkg` file from
||| its GitHub repository. The resulting pair contains also
||| the unmodified content of the `.ipkg` file.
export covering
resolvePair :  HasIO io
            => (e : Env s)
            -> PkgRep
            -> EitherT PackErr io (String, ResolvedPackage)
resolvePair e pr = do
  debug e "Trying to resolve package \{pr}"
  res <- resolveImpl e pr
  debug e "Found \{descStr $ snd res} \{name $ snd res}"
  pure res

||| Lookup a package name in the package data base,
||| then download and extract its `.ipkg` file from
||| its GitHub repository.
export covering
resolve :  HasIO io
        => (e : Env s)
        -> PkgRep
        -> EitherT PackErr io ResolvedPackage
resolve e pr = map snd (resolvePair e pr)
