module Pack.Runner.Database

import Data.SortedMap
import Idris.Package.Types
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
cacheCoreIpkgFiles : HasIO io => Env s -> Path Abs -> EitherT PackErr io ()
cacheCoreIpkgFiles e dir = for_ corePkgs $ \c =>
  copyFile (dir </> coreIpkgPath c) (coreCachePath e c)

resolveCore :  HasIO io
            => Env s
            -> CorePkg
            -> EitherT PackErr io (String, ResolvedPackage)
resolveCore e c =
  let pth := coreCachePath e c
   in do
     when !(missing pth) $
       withGit (tmpDir e) e.db.idrisURL e.db.idrisCommit (cacheCoreIpkgFiles e)
     parseIpkgFile pth (Core c)

covering
resolveImpl :  HasIO io
            => (e : Env s)
            -> PkgName
            -> EitherT PackErr io (String, ResolvedPackage)
resolveImpl e "base"    = resolveCore e Base
resolveImpl e "contrib" = resolveCore e Contrib
resolveImpl e "linear"  = resolveCore e Linear
resolveImpl e "idris2"  = resolveCore e IdrisApi
resolveImpl e "network" = resolveCore e Network
resolveImpl e "prelude" = resolveCore e Prelude
resolveImpl e "test"    = resolveCore e Test
resolveImpl e n         = case lookup n (allPackages e) of
  Nothing  => throwE (UnknownPkg n)

  -- this is a known package so we download its `.ipkg`
  -- file from GitHub and patch it, if necessary
  Just (GitHub url commit ipkg pp) =>
    let cache = ipkgPath e n commit ipkg
     in do
       when !(missing cache) $
         withGit (tmpDir e) url commit $ \dir => do
           let ipkgAbs := dir </> ipkg
               pf      := patchFile e n ipkg
           when !(exists pf) (patch ipkgAbs pf)
           copyFile ipkgAbs cache
       parseIpkgFile cache (RGitHub n url commit ipkg pp)
  Just (Local dir ipkg pp) =>
    parseIpkgFile (dir </> ipkg) (RLocal n dir ipkg pp)

execStr : PkgDesc -> String
execStr d = maybe "library" (const "application") d.executable

descStr : ResolvedPackage -> String
descStr (RGitHub name url commit ipkg _ desc) =
  "GitHub \{execStr desc} (\{url}:\{commit})"
descStr (RLocal name dir ipkg _ desc) = "local \{execStr desc}"
descStr _ = "core package"

||| Lookup a package name in the package data base,
||| then download and extract its `.ipkg` file from
||| its GitHub repository. The resulting pair contains also
||| the unmodified content of the `.ipkg` file.
export covering
resolvePair :  HasIO io
            => (e : Env s)
            -> PkgName
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
        -> PkgName
        -> EitherT PackErr io ResolvedPackage
resolve e pr = map snd (resolvePair e pr)
