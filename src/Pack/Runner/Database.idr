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
          -> EitherT PackErr io (LibStatus p d)
libStatus e n p d = case isLib d of
  No c     => pure NoLib
  Yes plib => do
    True <- exists (pkgInstallDir e n p d) | False => pure Missing
    b    <- exists $ pkgDocs e n p
    case isLocal p of
      No c     => pure $ Installed b
      Yes ploc =>
        let ts  := libTimestamp e n p
            dir := localSrcDir p d
         in checkOutdated ts dir Outdated (Installed b)

-- checks the status of an application
appStatus :  HasIO io
          => Env e
          -> PkgName
          -> (p : Package)
          -> (d : PkgDesc)
          -> EitherT PackErr io (AppStatus p d)
appStatus e n p d with (exec d) proof eq
  _ | Nothing  = pure NoApp
  _ | Just exe = do
    True <- fileExists (pkgExec e n p exe d) | False => pure $ Missing exe
    case isLocal p of
      No c     => pure $ AppInstalled exe
      Yes ploc =>
        let ts  := appTimestamp e n p
            src := localSrcDir p d
         in checkOutdated ts src (Outdated exe) (AppInstalled exe)

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
resolve : HasIO io => Env s -> PkgName -> EitherT PackErr io ResolvedPackage
resolve e n = case lookup n (allPackages e) of
  Nothing => throwE (UnknownPkg n)
  Just p  => do
    (str,d) <- loadIpkg e n p
    lib     <- libStatus e n p d
    app     <- appStatus e n p d
    pure $ MkRP p n d str lib app

needsInstalling : (PkgType, ResolvedPackage) -> Bool
needsInstalling (Lib, rp) = case rp.lib of
  NoLib       => False
  Missing     => True
  Installed _ => False
  Outdated    => True

needsInstalling (Bin, rp) = case rp.app of
  NoApp            => False
  Missing _        => True
  AppInstalled _   => False
  Outdated _       => True

showPlan : List (PkgType, PkgName) -> String
showPlan = unlines . map (\(t,n) => "\{n} (\{t})")

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
   in do
     debug e "Building plan for the following libraries: \n \{showPlan ps}"
     filter needsInstalling <$> go empty Lin ps'
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
