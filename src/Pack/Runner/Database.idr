module Pack.Runner.Database

import Core.FC
import Core.Name.Namespace
import Data.SortedMap
import Idris.Package.Types
import Pack.Config.Env
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types

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
safe : HasIO io => Env s -> Desc t -> EitherT PackErr io (Desc Safe)
safe c (MkDesc d s f _) =
  let unsafe := isJust
             $   d.prebuild
             <|> d.postbuild
             <|> d.preinstall
             <|> d.postinstall
   in case c.safetyPrompt && unsafe of
        False => pure $ MkDesc d s f IsSafe
        True  => do
          warn c "Package \{name d} uses custom build hooks. Continue (yes/*no)?"
          "yes" <- trim <$> getLine | _ => throwE SafetyAbort
          pure $ MkDesc d s f IsSafe

||| Like `safe` but verify also that the package does not
||| produce an executable called "pack"
export
notPack : HasIO io => Env s -> Desc t -> EitherT PackErr io (Desc NotPack)
notPack e d = do
  MkDesc d s f p <- safe e d
  case d.executable of
    Just "pack" => throwE ManualInstallPackApp
    _           => pure $ MkDesc d s f (IsNotPack p)

||| Parse an `.ipkg` file and check if it has custom build hooks.
export
parseLibIpkg :  HasIO io
             => Env s
             -> (file : File Abs)
             -> (loc : File Abs)
             -> EitherT PackErr io (Desc Safe)
parseLibIpkg e p loc = parseIpkgFile p loc >>= safe e

export
parseAppIpkg :  HasIO io
             => Env s
             -> (file : File Abs)
             -> (loc : File Abs)
             -> EitherT PackErr io (Desc NotPack)
parseAppIpkg e p loc = parseIpkgFile p loc >>= notPack e

export
safeLib : HasIO io => Env s -> ResolvedLib t -> EitherT PackErr io SafeLib
safeLib e l  = reTag l <$> safe e l.desc

export
safeApp : HasIO io => Env s -> ResolvedApp t -> EitherT PackErr io SafeApp
safeApp e a = reTag a <$> notPack e a.desc

export
checkLOA : HasIO io => Env s -> LibOrApp U -> EitherT PackErr io SafePkg
checkLOA e (Left x)  = Left <$> safeLib e x
checkLOA e (Right x) = Right <$> safeApp e x

--------------------------------------------------------------------------------
--          Utils
--------------------------------------------------------------------------------

export
withCoreGit : HasIO io
            => Env e
            -> (Path Abs -> EitherT PackErr io a)
            -> EitherT PackErr io a
withCoreGit e = withGit (tmpDir e) compiler e.db.idrisURL e.db.idrisCommit

export
withPkgEnv :  HasIO io
             => Env e
             -> PkgName
             -> Package
             -> (Path Abs -> EitherT PackErr io a)
             -> EitherT PackErr io a
withPkgEnv e n (GitHub u c i _) f = withGit (tmpDir e) n u c f
withPkgEnv e n (Local d i _)    f = inDir d f
withPkgEnv e n (Core _)         f = withCoreGit e f

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
          => Env e
          -> PkgName
          -> (p : Package)
          -> (d : Desc t)
          -> EitherT PackErr io (PkgStatus p)
libStatus e n p d = do
  True <- exists (pkgInstallDir e n p d) | False => pure Missing
  b    <- exists $ pkgDocs e n p
  case isLocal p of
    No c     => pure $ Installed
    Yes ploc =>
      let ts  := libTimestamp e n p
          dir := localSrcDir d
       in checkOutdated ts dir Outdated Installed

-- checks the status of an application
appStatus :  HasIO io
          => Env e
          -> PkgName
          -> (p   : Package)
          -> (d   : Desc t)
          -> (exe : Body)
          -> EitherT PackErr io (PkgStatus p)
appStatus e n p d exe = do
  True <- fileExists (pkgExec e n p exe) | False => pure Missing
  case isLocal p of
    No c     => pure Installed
    Yes ploc =>
      let ts  := appTimestamp e n p
          src := localSrcDir d
       in checkOutdated ts src Outdated Installed

export
cacheCoreIpkgFiles : HasIO io => Env s -> Path Abs -> EitherT PackErr io ()
cacheCoreIpkgFiles e dir = for_ corePkgs $ \c =>
  copyFile (toAbsFile dir (coreIpkgPath c)) (coreCachePath e c)

loadIpkg :  HasIO io
         => Env s
         -> PkgName
         -> Package
         -> EitherT PackErr io (Desc U)
loadIpkg e n (GitHub u c i _) =
  let cache  := ipkgCachePath e n c i
      tmpLoc := gitDir (tmpDir e) n c </> i
   in do
     when !(fileMissing cache) $
       withGit (tmpDir e) n u c $ \dir => do
         let pf := patchFile e n i
         when !(fileExists pf) (patch tmpLoc pf)
         copyFile tmpLoc cache
     parseIpkgFile cache tmpLoc
loadIpkg e n (Local d i _)    = parseIpkgFile (d </> i) (d </> i)
loadIpkg e n (Core c)         =
  let cache  := coreCachePath e c
      tmpLoc := gitDir (tmpDir e) compiler  e.db.idrisCommit </> coreIpkgPath c
   in do
     when !(fileMissing cache) $ withCoreGit e (cacheCoreIpkgFiles e)
     parseIpkgFile cache tmpLoc

export covering
resolveLib : HasIO io => Env s -> PkgName -> EitherT PackErr io (ResolvedLib U)
resolveLib e n = case lookup n (allPackages e) of
  Nothing => throwE (UnknownPkg n)
  Just p  => do
    d   <- loadIpkg e n p
    lib <- libStatus e n p d
    pure $ RL p n d lib

export covering
resolveApp : HasIO io => Env s -> PkgName -> EitherT PackErr io (ResolvedApp U)
resolveApp e n = do
  RL p n d _ <- resolveLib e n
  case exec d of
    Nothing  => throwE (NoApp n)
    Just exe => (\s => RA p n d s exe) <$> appStatus e n p d exe

export covering
resolveAny :  HasIO io
           => Env s
           -> PkgType
           -> PkgName
           -> EitherT PackErr io (LibOrApp U)
resolveAny e Lib n = Left  <$> resolveLib e n
resolveAny e Bin n = Right <$> resolveApp e n

export covering
appPath : HasIO io => PkgName -> Env s -> EitherT PackErr io ()
appPath "idris2" e = putStrLn "\{idrisExec e}"
appPath n e = do
  ra <- resolveApp e n
  putStrLn . interpolate $ pkgExec e ra.name ra.pkg ra.exec

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
     => Env s
     -> List (PkgType, PkgName)
     -> EitherT PackErr io (List SafePkg)
plan e ps =
  let ps' := Right <$> (Lib, "prelude") :: (Lib, "base") :: ps
   in do
     debug e "Building plan for the following libraries: \n \{showPlan ps}"
     loas <- filter needsInstalling <$> go empty Lin ps'
     traverse (checkLOA e) loas
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
            loa <- resolveAny e (fst p) (snd p)
            let deps := (\d => Right (Lib, d)) <$> dependencies loa
            go ps sx $ deps ++ Left loa :: xs
