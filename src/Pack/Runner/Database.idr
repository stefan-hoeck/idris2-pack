module Pack.Runner.Database

import Core.FC
import Core.Name.Namespace
import Data.SortedMap
import Idris.Package.Types
import Pack.Config
import Pack.Core
import Pack.Database

%default total

--------------------------------------------------------------------------------
--          Safe
--------------------------------------------------------------------------------

||| Proof that the safety aspects of a value have been checked.
||| Right now, this is restricted to `.ipkg` files (and thus, resolved
||| libs and apps) with custom build and install hooks.
export
data Safe : PkgDesc -> Type where
  IsSafe : Safe d

public export
0 SafeLib : Type
SafeLib = ResolvedLib Safe

||| Proof that an app is not pack, i.e. does not have an
||| executable called pack. We want to make sure that
||| users don't inadvertently overwrite the pack installation.
export
data NotPack : PkgDesc -> Type where
  IsNotPack : Safe d -> NotPack d

export
0 toSafe : NotPack d -> Safe d
toSafe (IsNotPack s) = s

public export
0 SafeApp : Type
SafeApp = ResolvedApp NotPack

public export
0 SafePkg : Type
SafePkg = Either SafeLib SafeApp

export
notPackIsSafe : Desc NotPack -> Desc Safe
notPackIsSafe (MkDesc x y z s) = MkDesc x y z $ toSafe s

||| check if a package has special build- or install hooks
||| defined, and if yes, prompt the user
||| before continuing (unless `safetyPrompt` in the
||| `Config` is set set to `False`).
export
safe : HasIO io => (e : Env) => Desc t -> EitherT PackErr io (Desc Safe)
safe (MkDesc d s f _) =
  let unsafe := isJust
             $   d.prebuild
             <|> d.postbuild
             <|> d.preinstall
             <|> d.postinstall
   in case e.config.safetyPrompt && unsafe of
        False => pure $ MkDesc d s f IsSafe
        True  => do
          warn "Package \{name d} uses custom build hooks. Continue (yes/*no)?"
          "yes" <- trim <$> getLine | _ => throwE SafetyAbort
          pure $ MkDesc d s f IsSafe

||| Like `safe` but verify also that the package does not
||| produce an executable called "pack"
export
notPack : HasIO io => Env => Desc t -> EitherT PackErr io (Desc NotPack)
notPack d = do
  MkDesc d s f p <- safe d
  case d.executable of
    Just "pack" => throwE ManualInstallPackApp
    _           => pure $ MkDesc d s f (IsNotPack p)

||| Parse an `.ipkg` file and check if it has custom build hooks.
export
parseLibIpkg :  HasIO io
             => (e : Env)
             => (file : File Abs)
             -> (loc : File Abs)
             -> EitherT PackErr io (Desc Safe)
parseLibIpkg p loc = parseIpkgFile p loc >>= safe

export
parseAppIpkg :  HasIO io
             => Env
             => (file : File Abs)
             -> (loc : File Abs)
             -> EitherT PackErr io (Desc NotPack)
parseAppIpkg p loc = parseIpkgFile p loc >>= notPack

export
safeLib : HasIO io => Env => ResolvedLib t -> EitherT PackErr io SafeLib
safeLib l  = reTag l <$> safe l.desc

export
safeApp : HasIO io => Env => ResolvedApp t -> EitherT PackErr io SafeApp
safeApp a = reTag a <$> notPack a.desc

export
checkLOA : HasIO io => Env => LibOrApp U -> EitherT PackErr io SafePkg
checkLOA (Left x)  = Left <$> safeLib x
checkLOA (Right x) = Right <$> safeApp x

--------------------------------------------------------------------------------
--          Resolving Packages
--------------------------------------------------------------------------------

export
withCoreGit : HasIO io
            => (e : Env)
            => (Path Abs -> EitherT PackErr io a)
            -> EitherT PackErr io a
withCoreGit = withGit tmpDir compiler e.db.idrisURL e.db.idrisCommit

export
withPkgEnv :  HasIO io
             => Env
             => PkgName
             -> Package
             -> (Path Abs -> EitherT PackErr io a)
             -> EitherT PackErr io a
withPkgEnv n (GitHub u c i _) f = withGit tmpDir n u c f
withPkgEnv n (Local d i _)    f = inDir d f
withPkgEnv n (Core _)         f = withCoreGit f

-- tests if the files in a directory are new compare to
-- a timestamp file, and returns one of two possible results
-- accordingly
checkOutdated :  HasIO io
              => (ts         : File Abs)
              -> (dir        : Path Abs)
              -> (ifOutdated : a)
              -> (ifUpToDate : a)
              -> EitherT PackErr io a
checkOutdated ts src o u = do
  True <- fileExists ts | False => pure o
  ""   <- trim <$> sysRun "find \{src} -newer \{ts}" | _ => pure o
  pure u

-- checks the status of a library
libStatus :  HasIO io
          => Env
          => PkgName
          -> (p : Package)
          -> (d : Desc t)
          -> EitherT PackErr io (PkgStatus p)
libStatus n p d = do
  True <- exists (pkgInstallDir n p d) | False => pure Missing
  b    <- exists $ pkgDocs n p
  case isLocal p of
    No c     => pure $ Installed
    Yes ploc =>
      let ts  := libTimestamp n p
          dir := localSrcDir d
       in checkOutdated ts dir Outdated Installed

export
appStatus :  HasIO io
          => Env
          => PkgName
          -> (p   : Package)
          -> (d   : Desc t)
          -> (exe : Body)
          -> EitherT PackErr io (PkgStatus p)
appStatus n p d exe = do
  True <- fileExists (pkgExec n p exe) | False => pure Missing
  case isLocal p of
    No c     => pure Installed
    Yes ploc =>
      let ts  := appTimestamp n p
          src := localSrcDir d
       in checkOutdated ts src Outdated Installed

export
cacheCoreIpkgFiles : HasIO io => Env => Path Abs -> EitherT PackErr io ()
cacheCoreIpkgFiles dir = for_ corePkgs $ \c =>
  copyFile (toAbsFile dir (coreIpkgPath c)) (coreCachePath c)

loadIpkg :  HasIO io
         => (e : Env)
         => PkgName
         -> Package
         -> EitherT PackErr io (Desc U)
loadIpkg n (GitHub u c i _) =
  let cache  := ipkgCachePath n c i
      tmpLoc := gitDir tmpDir n c </> i
   in do
     when !(fileMissing cache) $
       withGit tmpDir n u c $ \dir => do
         let pf := patchFile n i
         when !(fileExists pf) (patch tmpLoc pf)
         copyFile tmpLoc cache
     parseIpkgFile cache tmpLoc
loadIpkg n (Local d i _)    = parseIpkgFile (d </> i) (d </> i)
loadIpkg n (Core c)         =
  let cache  := coreCachePath c
      tmpLoc := gitDir tmpDir compiler  e.db.idrisCommit </> coreIpkgPath c
   in do
     when !(fileMissing cache) $ withCoreGit cacheCoreIpkgFiles
     parseIpkgFile cache tmpLoc

export covering
resolveLib : HasIO io => Env => PkgName -> EitherT PackErr io (ResolvedLib U)
resolveLib n = case lookup n allPackages of
  Nothing => throwE (UnknownPkg n)
  Just p  => do
    d   <- loadIpkg n p
    lib <- libStatus n p d
    pure $ RL p n d lib

export covering
resolveApp : HasIO io => Env => PkgName -> EitherT PackErr io (ResolvedApp U)
resolveApp n = do
  RL p n d _ <- resolveLib n
  case exec d of
    Nothing  => throwE (NoApp n)
    Just exe => (\s => RA p n d s exe) <$> appStatus n p d exe

export covering
resolveAny :  HasIO io
           => Env
           => PkgType
           -> PkgName
           -> EitherT PackErr io (LibOrApp U)
resolveAny Lib n = Left  <$> resolveLib n
resolveAny Bin n = Right <$> resolveApp n

export covering
appPath : HasIO io => PkgName -> Env -> EitherT PackErr io ()
appPath "idris2" e = putStrLn "\{idrisExec}"
appPath n e = do
  ra <- resolveApp n
  putStrLn . interpolate $ pkgExec ra.name ra.pkg ra.exec

--------------------------------------------------------------------------------
--         Installation Plan
--------------------------------------------------------------------------------

needsInstalling' : PkgStatus p -> Bool
needsInstalling' Missing   = True
needsInstalling' Installed = False
needsInstalling' Outdated  = True

needsInstalling : LibOrApp t -> Bool
needsInstalling (Left x)  = needsInstalling' x.status
needsInstalling (Right x) = needsInstalling' x.status

showPlan : List (PkgType, PkgName) -> String
showPlan = unlines . map (\(t,n) => "\{t} \{n}")

||| Create a build plan for the given list of packages and apps
||| plus their dependencies.
|||
||| All packages depend on the prelude and
||| base, so we make sure these are installed as well.
export covering
plan :  HasIO io
     => Env
     => List (PkgType, PkgName)
     -> EitherT PackErr io (List SafePkg)
plan ps =
  let ps' := Right <$> (Lib, "prelude") :: (Lib, "base") :: ps
   in do
     debug "Building plan for the following libraries: \n \{showPlan ps}"
     loas <- filter needsInstalling <$> go empty Lin ps'
     traverse checkLOA loas
  where covering
        go :  (planned  : SortedMap (PkgType, PkgName) ())
           -> (resolved : SnocList $ LibOrApp U)
           -> List (Either (LibOrApp U) (PkgType,PkgName))
           -> EitherT PackErr io (List $ LibOrApp U)
        go ps sx []             = pure (sx <>> [])

        -- the lib or app has already been resolved and
        -- its dependencies are already part of the build plan `sx`
        -- We make sure its not added as a dep again and add
        -- it to the list of handled packages.
        go ps sx (Left (Left lib) :: xs) =
          go (insert (Lib, name lib) () ps) (sx :< Left lib) xs

        go ps sx (Left (Right app) :: xs) =
          go (insert (Bin, name app) () ps) (sx :< Right app) xs

        -- the package `p` might have been resolved already
        -- if it was a dependency of another package
        -- if that's the case, we ignore it. Otherwise, we
        go ps sx (Right p :: xs) = case lookup p ps of
          Just ()  => go ps sx xs
          Nothing  => do
            loa <- resolveAny (fst p) (snd p)
            let deps := (\d => Right (Lib, d)) <$> dependencies loa
            go ps sx $ deps ++ Left loa :: xs
