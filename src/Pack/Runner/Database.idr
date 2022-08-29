module Pack.Runner.Database

import Core.FC
import Core.Name.Namespace
import Data.SortedMap
import Data.IORef
import Idris.Package.Types
import Pack.Config
import Pack.Core
import Pack.Database

%default total

--------------------------------------------------------------------------------
--          State
--------------------------------------------------------------------------------

||| Cache used during package resolution
public export
0 LibCache : Type
LibCache = IORef (SortedMap PkgName $ ResolvedLib U)

export
emptyCache : HasIO io => io LibCache
emptyCache = newIORef SortedMap.empty

cache :  HasIO io
      => (ref : LibCache)
      => PkgName
      -> ResolvedLib U
      -> io (ResolvedLib U)
cache n lib = modifyIORef ref (insert n lib) $> lib

--------------------------------------------------------------------------------
--          Safe
--------------------------------------------------------------------------------

||| Proof that the safety aspects of a value have been checked.
||| Right now, this is restricted to `.ipkg` files (and thus, resolved
||| libs and apps) with custom build and install hooks.
export
data Safe : PkgDesc -> Type where
  IsSafe : Safe d

||| Alias for `ResolvedLib Safe`
public export
0 SafeLib : Type
SafeLib = ResolvedLib Safe

||| Proof that an app is not pack, i.e. does not have an
||| executable called pack. We want to make sure that
||| users don't inadvertently overwrite the pack installation.
export
data NotPack : PkgDesc -> Type where
  IsNotPack : Safe d -> NotPack d

||| Extract the `Safe d` wrapped in a `NotPack d`
export
0 toSafe : NotPack d -> Safe d
toSafe (IsNotPack s) = s

||| Alias for `ResolvedApp Safe`
public export
0 SafeApp : Type
SafeApp = ResolvedApp NotPack

||| Alias for `Either SafeLib SafeApp`
public export
0 SafePkg : Type
SafePkg = LibOrApp Safe NotPack

||| This holds, sind `NotPack d` implies `Safe d` as shown by
||| `toSafe`.
export
notPackIsSafe : Desc NotPack -> Desc Safe
notPackIsSafe (MkDesc x y z s) = MkDesc x y z $ toSafe s

||| Check if a package has special build- or install hooks
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

||| Parse an `.ipkg` file from a command line arg or a local
||| package given as a package name and check if it has custom build hooks.
export
findAndParseLocalIpkg :  HasIO io
                      => (e    : Env)
                      => (file : Either (File Abs) PkgName)
                      -> EitherT PackErr io (Desc Safe)
findAndParseLocalIpkg (Left p)  = parseLibIpkg p p
findAndParseLocalIpkg (Right n) =
  case lookup n allPackages of
    Nothing                 => throwE (UnknownPkg n)
    Just (Local dir ipkg _) => let p = dir </> ipkg in parseLibIpkg p p
    Just _                  => throwE (NotLocalPkg n)


||| Parse an `.ipkg` file representing an application
||| and check if it has no custom build hooks and does not produce
||| an executable called "pack".
export
parseAppIpkg :  HasIO io
             => Env
             => (file : File Abs)
             -> (loc : File Abs)
             -> EitherT PackErr io (Desc NotPack)
parseAppIpkg p loc = parseIpkgFile p loc >>= notPack

||| Check if a resolved library is safe to install (prompt, if it uses
||| custom build hooks in its `.ipkg` file).
export
safeLib : HasIO io => Env => ResolvedLib t -> EitherT PackErr io SafeLib
safeLib l  = reTag l <$> safe l.desc

||| Check if a resolved app is safe to install (prompt, if it uses
||| custom build hooks in its `.ipkg` file).
|||
||| This fails if the app produces an executable called "pack": We don't
||| want to overwrite the pack installation.
export
safeApp : HasIO io => Env => ResolvedApp t -> EitherT PackErr io SafeApp
safeApp a = reTag a <$> notPack a.desc

||| Runs `safeLib` or `safeApp` on a library or app.
export
checkLOA : HasIO io => Env => LibOrApp U U -> EitherT PackErr io SafePkg
checkLOA (Lib x)   = Lib <$> safeLib x
checkLOA (App b x) = App b <$> safeApp x

--------------------------------------------------------------------------------
--          Resolving Packages
--------------------------------------------------------------------------------

||| Run a pack action in the directory of the cloned Idris repository.
export
withCoreGit : HasIO io
            => (e : Env)
            => (Path Abs -> EitherT PackErr io a)
            -> EitherT PackErr io a
withCoreGit = withGit tmpDir compiler e.db.idrisURL e.db.idrisCommit

||| Run a pack action in the directory of a (possibly cloned) package.
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

isOutdated : DPair Package PkgStatus -> Bool
isOutdated (fst ** Outdated) = True
isOutdated _                 = False

-- tests if the files in a directory are new compare to
-- a timestamp file, and returns one of two possible results
-- accordingly
checkOutdated :  HasIO io
              => (ts         : File Abs)
              -> (dir        : Path Abs)
              -> (deps       : List (DPair Package PkgStatus))
              -> (ifOutdated : a)
              -> (ifUpToDate : a)
              -> EitherT PackErr io a
checkOutdated ts src deps o u =
  if any isOutdated deps
    then pure o
    else do
      True <- fileExists ts | False => pure o
      ""   <- trim <$> sysRun "find \{src} -newer \{ts}" | _ => pure o
      pure u

-- checks the status of a library
libStatus :  HasIO io
          => Env
          => PkgName
          -> (p    : Package)
          -> (d    : Desc t)
          -> (deps : List (DPair Package PkgStatus))
          -> EitherT PackErr io (PkgStatus p)
libStatus n p d deps = do
  True <- exists (pkgInstallDir n p d) | False => pure Missing
  b    <- exists $ pkgDocs n p
  case isLocal p of
    No c     => pure $ Installed
    Yes ploc =>
      let ts  := libTimestamp n p
          dir := localSrcDir d
       in checkOutdated ts dir deps Outdated Installed

||| Generates the `AppStatus` of a package representing an application.
export
appStatus :  HasIO io
          => Env
          => PkgName
          -> (p    : Package)
          -> (d    : Desc t)
          -> (deps : List (DPair Package PkgStatus))
          -> (exe  : Body)
          -> EitherT PackErr io (AppStatus p)
appStatus n p d deps exe = do
  True      <- fileExists (pkgExec n p exe) | False => pure Missing
  installed <- do
    True <- fileExists (pathExec exe) | False => pure Installed
    pure BinInstalled
  case isLocal p of
    No c     => pure installed
    Yes ploc =>
      let ts  := appTimestamp n p
          src := localSrcDir d
       in checkOutdated ts src deps Outdated installed

||| Caches the `.ipkg` files of the core libraries to make them
||| quickly available when running queries.
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

||| Try to fully resolve a library given as a package name.
||| This will look up the library in the current package collection
||| and will fetch and read its (possibly cached) `.ipkg` file.
export covering
resolveLib :  HasIO io
           => (ref : LibCache)
           => Env
           => PkgName
           -> EitherT PackErr io (ResolvedLib U)
resolveLib n = do
  Nothing <- lookup n <$> readIORef ref | Just pkg => pure pkg
  case lookup n allPackages of
    Nothing => throwE (UnknownPkg n)
    Just p  => do
      d    <- loadIpkg n p
      deps <- traverse resolveDep $ dependencies d
      lib  <- libStatus n p d deps
      cache n $ RL p n d lib deps

  where
    resolveDep : PkgName -> EitherT PackErr io (DPair Package PkgStatus)
    resolveDep n = do
      rl <- resolveLib n
      pure (MkDPair rl.pkg rl.status)

||| Try to fully resolve an application given as a package name.
||| This will look up the app in the current package collection
||| and will fetch and read its (possibly cached) `.ipkg` file.
export covering
resolveApp :  HasIO io
           => LibCache
           => Env
           => PkgName
           -> EitherT PackErr io (ResolvedApp U)
resolveApp n = do
  RL p n d _ ds <- resolveLib n
  case exec d of
    Nothing  => throwE (NoApp n)
    Just exe => do
      (\s => RA p n d s exe ds) <$> appStatus n p d ds exe

||| Try to fully resolve an application or library given as a package.
||| This will look up the package name in the current package collection
||| and will fetch and read its (possibly cached) `.ipkg` file.
export covering
resolveAny :  HasIO io
           => LibCache
           => Env
           => InstallType
           -> PkgName
           -> EitherT PackErr io (LibOrApp U U)
resolveAny Library n = Lib   <$> resolveLib n
resolveAny (App b) n = App b <$> resolveApp n

||| Prints the absolute path of an application's installed executable
||| to stdout. This is used in the wrapper scripts we use for invoking
||| the correct version of apps such as `idris2-lsp`, the version of
||| which depend on the `pack.toml` files currently in scope.
export covering
appPath : HasIO io => PkgName -> Env -> EitherT PackErr io ()
appPath "idris2" e = putStrLn "\{idrisExec}"
appPath n e = do
  ref <- emptyCache
  ra <- resolveApp n
  putStrLn . interpolate $ pkgExec ra.name ra.pkg ra.exec

--------------------------------------------------------------------------------
--         Installation Plan
--------------------------------------------------------------------------------

pkgNeedsInstalling : PkgStatus p -> Bool
pkgNeedsInstalling Missing   = True
pkgNeedsInstalling Installed = False
pkgNeedsInstalling Outdated  = True

appNeedsInstalling : (withWrapperScript : Bool) -> AppStatus p -> Bool
appNeedsInstalling _ Missing      = True
appNeedsInstalling b Installed    = b
appNeedsInstalling _ Outdated     = True
appNeedsInstalling _ BinInstalled = False

needsInstalling : LibOrApp t s -> Bool
needsInstalling (Lib x)   = pkgNeedsInstalling x.status
needsInstalling (App b x) = appNeedsInstalling b x.status

showPlan : List (InstallType, PkgName) -> String
showPlan = unlines . map (\(t,n) => "\{t} \{n}")

||| Create a build plan for the given list of packages and apps
||| plus their dependencies.
|||
||| All packages depend on the prelude and
||| base, so we make sure these are installed as well.
export covering
plan :  HasIO io
     => LibCache
     => Env
     => List (InstallType, PkgName)
     -> EitherT PackErr io (List SafePkg)
plan ps =
  let ps' := Right <$> (Library, "prelude") :: (Library, "base") :: ps
   in do
     debug "Building plan for the following libraries: \n \{showPlan ps}"
     loas <- filter needsInstalling <$> go empty Lin ps'
     traverse checkLOA loas
  where covering
        go :  (planned  : SortedMap (InstallType, PkgName) ())
           -> (resolved : SnocList $ LibOrApp U U)
           -> List (Either (LibOrApp U U) (InstallType,PkgName))
           -> EitherT PackErr io (List $ LibOrApp U U)
        go ps sx []             = pure (sx <>> [])

        -- the lib or app has already been resolved and
        -- its dependencies are already part of the build plan `sx`
        -- We make sure its not added as a dep again and add
        -- it to the list of handled packages.
        go ps sx (Left (Lib lib) :: xs) =
          go (insert (Library, name lib) () ps) (sx :< Lib lib) xs

        go ps sx (Left (App b app) :: xs) =
          go (insert (App b, name app) () ps) (sx :< App b app) xs

        -- the package `p` might have been resolved already
        -- if it was a dependency of another package
        -- if that's the case, we ignore it. Otherwise, we
        go ps sx (Right p :: xs) = case lookup p ps of
          Just ()  => go ps sx xs
          Nothing  => do
            loa <- resolveAny (fst p) (snd p)
            let deps := (\d => Right (Library, d)) <$> dependencies loa
            go ps sx $ deps ++ Left loa :: xs
