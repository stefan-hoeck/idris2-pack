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
       warn c "Package \{n} uses custom build hooks. Continue (yes/*no)?"
       "yes" <- trim <$> getLine | _ => putStrLn "Aborting..." $> False
       pure True
     else pure True

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
          -> (d : PkgDesc)
          -> EitherT PackErr io (PkgStatus p)
libStatus e n p d = do
  True <- exists (pkgInstallDir e n p d) | False => pure Missing
  b    <- exists $ pkgDocs e n p
  case isLocal p of
    No c     => pure $ Installed
    Yes ploc =>
      let ts  := libTimestamp e n p
          dir := localSrcDir p d
       in checkOutdated ts dir Outdated Installed

-- checks the status of an application
appStatus :  HasIO io
          => Env e
          -> PkgName
          -> (p   : Package)
          -> (d   : PkgDesc)
          -> (exe : Body)
          -> EitherT PackErr io (PkgStatus p)
appStatus e n p d exe = do
  True <- fileExists (pkgExec e n p exe) | False => pure Missing
  case isLocal p of
    No c     => pure Installed
    Yes ploc =>
      let ts  := appTimestamp e n p
          src := localSrcDir p d
       in checkOutdated ts src Outdated Installed

export
cacheCoreIpkgFiles : HasIO io => Env s -> Path Abs -> EitherT PackErr io ()
cacheCoreIpkgFiles e dir = for_ corePkgs $ \c =>
  copyFile (toAbsFile dir (coreIpkgPath c)) (coreCachePath e c)

loadIpkg :  HasIO io
         => Env s
         -> PkgName
         -> Package
         -> EitherT PackErr io (String, PkgDesc)
loadIpkg e n (GitHub u c i _) =
  let cache = ipkgPath e n c i
   in do
     when !(fileMissing cache) $
       withGit (tmpDir e) n u c $ \dir => do
         let ipkgAbs := toAbsFile dir i
             pf      := patchFile e n i
         when !(fileExists pf) (patch ipkgAbs pf)
         copyFile ipkgAbs cache
     parseIpkgFile cache id
loadIpkg e n (Local d i _)    = parseIpkgFile (d </> i) id
loadIpkg e n (Core c)         =
  let pth := coreCachePath e c
   in do
     when !(fileMissing pth) $ withCoreGit e (cacheCoreIpkgFiles e)
     parseIpkgFile pth id

export covering
resolveLib : HasIO io => Env s -> PkgName -> EitherT PackErr io ResolvedLib
resolveLib e n = case lookup n (allPackages e) of
  Nothing => throwE (UnknownPkg n)
  Just p  => do
    (str,d) <- loadIpkg e n p
    lib     <- libStatus e n p d
    pure $ RL p n d str lib

export covering
resolveApp : HasIO io => Env s -> PkgName -> EitherT PackErr io ResolvedApp
resolveApp e "pack" = throwE ManualInstallPackApp
resolveApp e n = case lookup n (allPackages e) of
  Nothing => throwE (UnknownPkg n)
  Just p  => do
    (str,d)  <- loadIpkg e n p
    Just exe <- pure (exec d) | Nothing => throwE (NoApp n)
    app      <- appStatus e n p d exe
    pure $ RA p n d str app exe

export covering
resolveAny :  HasIO io
           => Env s
           -> PkgType
           -> PkgName
           -> EitherT PackErr io LibOrApp
resolveAny e Lib n = Left  <$> resolveLib e n
resolveAny e Bin n = Right <$> resolveApp e n

export covering
appPath : HasIO io => PkgName -> Env s -> EitherT PackErr io ()
appPath n e = do
  ra <- resolveApp e n
  putStrLn . interpolate $ pkgExec e ra.name ra.pkg ra.exec

needsInstalling' : PkgStatus p -> Bool
needsInstalling' Missing   = True
needsInstalling' Installed = False
needsInstalling' Outdated  = True

needsInstalling : LibOrApp -> Bool
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
     -> EitherT PackErr io (List LibOrApp)
plan e ps =
  let ps' := Right <$> (Lib, "prelude") :: (Lib, "base") :: ps
   in do
     debug e "Building plan for the following libraries: \n \{showPlan ps}"
     filter needsInstalling <$> go empty Lin ps'
  where covering
        go :  (planned  : SortedMap (PkgType, PkgName) ())
           -> (resolved : SnocList LibOrApp)
           -> List (Either LibOrApp (PkgType,PkgName))
           -> EitherT PackErr io (List LibOrApp)
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
