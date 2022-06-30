module Pack.Runner.Database

import Core.FC
import Data.SortedMap
import Idris.Package.Types
import Pack.Config.Env
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types

%default total

-- check if a package has special build- or install hooks
-- defined, and if yes, prompt the user
-- before continuing (unless `safetyPrompt` in the
-- `Config` is set set to `False`).
prompt : HasIO io => PkgName -> Config s -> PkgDesc -> io Bool
prompt n c d =
  if c.safetyPrompt && isJust (
       d.prebuild <|> d.postbuild <|>
       d.preinstall <|> d.postinstall
     )
     then do
       putStrLn "Package \{n} uses custom build hooks. Continue (yes/*no)?"
       "yes" <- trim <$> getLine | _ => putStrLn "Aborting..." $> False
       pure True
     else pure True

export
coreGitDir : Env e -> Path Abs
coreGitDir e = gitDir (tmpDir e) compiler e.db.idrisCommit

export
withCoreGit : HasIO io
            => Env e
            -> (Path Abs -> EitherT PackErr io a)
            -> EitherT PackErr io a
withCoreGit e = withGit (tmpDir e) compiler e.db.idrisURL e.db.idrisCommit

-- check if the source dir of a local package has been
-- changed since the last time the package was installed
-- (by comparing with a timestamp).
localUpToDate :  HasIO io
              => (env : Env s)
              -> (p : ResolvedPackage)
              -> {auto 0 prf : IsLocal p}
              -> EitherT PackErr io Bool
localUpToDate env p =
  let ts  := localTimestamp env p
      src := localSrcDir p
   in do
     debug env "Checking files in \{src} against timestamp \{ts}."
     True <- fileExists ts
       | False => debug env "Timestamp not found." $> False
     True <- exists src
       | False => debug env "Source dir not found." $> False
     out  <- sysRun "find \{src} -newer \{ts}"
     debug env "Found: \{out}"
     pure . null $ trim out

||| Check if a package has already been built and installed
export
packageUpToDate :  HasIO io
                => (env : Env s)
                -> ResolvedPackage
                -> EitherT PackErr io Bool
packageUpToDate env p@(RLocal {}) = localUpToDate env p
packageUpToDate env p =
  let dir = packageInstallDir env p
   in do
       debug env "Looking for package \{name p} at \{dir}"
       exists dir

||| Check if the executable of a package has already been
||| built and installed
export
executableExists :  HasIO io
                 => Env s
                 -> File Abs
                 -> EitherT PackErr io Bool
executableExists c f =
  debug c "Looking for executable \{f.file} at \{f.parent}" >>
  fileExists f

||| Check if the given library / application already exists
||| and is up to date.
export
doInstall : HasIO io
          => Env s
          -> (PkgType, ResolvedPackage)
          -> EitherT PackErr io Bool
doInstall e (Lib, rp)        = do
  False <- packageUpToDate e rp | True => pure False
  prompt (name rp) e (desc rp)
doInstall e (Bin, RLocal {}) = pure True
doInstall e (Bin, rp)        = case packageExec e rp of
  Just exe => do
    False <- executableExists e exe | True => pure False
    prompt (name rp) e (desc rp)
  Nothing  => throwE (NoApp $ name rp)



export
cacheCoreIpkgFiles : HasIO io => Env s -> Path Abs -> EitherT PackErr io ()
cacheCoreIpkgFiles e dir = for_ corePkgs $ \c =>
  copyFile (toAbsFile dir (coreIpkgPath c)) (coreCachePath e c)

resolveCore :  HasIO io
            => Env s
            -> CorePkg
            -> EitherT PackErr io (String, ResolvedPackage)
resolveCore e c =
  let pth := coreCachePath e c
   in do
     when !(fileMissing pth) $ withCoreGit e (cacheCoreIpkgFiles e)
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
       when !(fileMissing cache) $
         withGit (tmpDir e) n url commit $ \dir => do
           let ipkgAbs := toAbsFile dir ipkg
               pf      := patchFile e n ipkg
           when !(fileExists pf) (patch ipkgAbs pf)
           copyFile ipkgAbs cache
       parseIpkgFile cache (RGitHub n url commit ipkg pp)
  Just (Local dir ipkg pp) =>
    let af = toAbsFile dir ipkg
     in parseIpkgFile af (RLocal n af pp)

execStr : PkgDesc -> String
execStr d = maybe "library" (const "application") d.executable

descStr : ResolvedPackage -> String
descStr (RGitHub name url commit ipkg _ desc) =
  "GitHub \{execStr desc} (\{url}:\{commit})"
descStr (RLocal name ipkg _ desc) = "local \{execStr desc}"
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

filterM : Monad m => (a -> m Bool) -> List a -> m (List a)
filterM f []        = pure []
filterM f (x :: xs) = do
  True <- f x | False => filterM f xs
  map (x ::) $ filterM f xs

||| Create a build plan for the given list of packages and apps
||| plus their dependencies.
|||
||| All packages depend on the prelude and
||| base, so we make sure these are installed as well.
export covering
plan :  HasIO io
     => Env s
     -> List (PkgType, PkgName)
     -> EitherT PackErr io (List (PkgType, ResolvedPackage))
plan e ps =
  let ps' := mapSnd Right <$> (Lib, "prelude") :: (Lib, "base") :: ps
   in go empty Lin ps' >>= filterM (doInstall e)
  where covering
        go :  (planned  : SortedMap (PkgType, PkgName) ())
           -> (resolved : SnocList (PkgType, ResolvedPackage))
           -> List (PkgType, Either ResolvedPackage PkgName)
           -> EitherT PackErr io (List (PkgType, ResolvedPackage))
        go ps sx []             = pure (sx <>> [])

        -- the package `rp` has already been resolved and
        -- its dependencies are already part of the build plan `sx`
        -- We make sure its not added as a dep again and add
        -- it to the list of handled packages.
        go ps sx ((t, Left rp) :: xs) =
          go (insert (t, name rp) () ps) (sx :< (t,rp)) xs

        -- the package `p` might have been resolved already
        -- if it was a dependency of another package
        -- if that's the case, we ignore it. Otherwise, we
        --
        go ps sx ((t, Right p) :: xs) = case lookup (t,p) ps of
          Just ()  => go ps sx xs
          Nothing  => do
            rp <- resolve e p
            let deps := (\d => (Lib, Right d)) <$> dependencies rp
            go ps sx $ deps ++ (t,Left rp) :: xs
