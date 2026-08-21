module Pack.Runner.Database

import Core.FC
import Core.Name.Namespace
import Data.SortedMap
import Data.IORef
import Idris.Package.Types
import Pack.Config
import Pack.Core
import Pack.Core.Hash
import Pack.Database
import Pack.Version as V

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
||| `Config` is set to `False` or the package's name is listed
||| in the whitelist of safe packages).
export
safe : HasIO io => (e : Env) => Desc t -> EitherT PackErr io (Desc Safe)
safe (MkDesc d s f _) =
  let unsafe :=
        isJust $
              d.prebuild
          <|> d.postbuild
          <|> d.preinstall
          <|> d.postinstall
   in case e.config.safetyPrompt &&
           unsafe &&
           not (MkPkgName d.name `elem` e.config.whitelist) of
        False => pure $ MkDesc d s f IsSafe
        True  => do
          let msg := "Package \{name d} uses custom build hooks."
          confirmOrAbort Warning msg
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
parseLibIpkg :
     {auto _ : HasIO io}
  -> {auto e : Env}
  -> (file : File Abs)
  -> (loc : File Abs)
  -> EitherT PackErr io (Desc Safe)
parseLibIpkg p loc = parseIpkgFile p loc >>= safe

||| Parse an `.ipkg` file from a command line arg or a local
||| package given as a package name and check if it has custom build hooks.
export
findAndParseLocalIpkg :
     {auto _ : HasIO io}
  -> {auto e: Env}
  -> (file : PkgOrIpkg)
  -> EitherT PackErr io (Desc Safe)
findAndParseLocalIpkg (Ipkg p) = parseLibIpkg p p
findAndParseLocalIpkg (Pkg n)  =
  case lookup n e.all of
    Nothing                   => throwE (UnknownPkg n)
    Just (Local dir ipkg _ _) => let p = dir </> ipkg in parseLibIpkg p p
    Just _                    => throwE (NotLocalPkg n)

||| Looks at the current directory and tries to find the only `.ipkg` file,
||| or fails with `AmbigIpkg` if it didn't manage to do so.
findTheOnlyIpkg :
     {auto _ : HasIO io}
  -> {auto _ : CurDir}
  -> EitherT PackErr io PkgOrIpkg
findTheOnlyIpkg = do
  [ipkg] <- filter isIpkgBody <$> entries curDir
    | lfs => throwE (BuildMany lfs)
  pure $ Ipkg $ curDir /> ipkg

||| Returns present package, or else tries to find the only loca `.ipkg` file.
export
refinePkg :
     {auto _ : HasIO io}
  -> {auto _ : CurDir}
  -> Maybe PkgOrIpkg
  -> EitherT PackErr io PkgOrIpkg
refinePkg (Just p) = pure p
refinePkg Nothing  = findTheOnlyIpkg

||| Parse an `.ipkg` file representing an application
||| and check if it has no custom build hooks and does not produce
||| an executable called "pack".
export
parseAppIpkg :
     {auto _ : HasIO io}
  -> {auto _ : Env}
  -> (file : File Abs)
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

||| Run a pack action in the directory of a (possibly cloned) package.
export
withPkgEnv :
     {auto _ : HasIO io}
  -> {auto _ : Env}
  -> PkgName
  -> Package
  -> (Path Abs -> EitherT PackErr io a)
  -> EitherT PackErr io a
withPkgEnv n (Git u c i _ _ _) f = withGit n u c False f
withPkgEnv n (Local d i _ _)   f = inDir d f
withPkgEnv n (Core _)          f = withCoreGit f

newerSrc : HasIO io => File Abs -> Path Abs -> EitherT PackErr io String
newerSrc ts src = trim <$> sysRun ["find", src, "-newer", ts]

newerIpkg : HasIO io => File Abs -> File Abs -> EitherT PackErr io String
newerIpkg ts ipkg =
  trim <$> sysRun ["find", ipkg.parent, "-name", "\{ipkg.file}", "-newer", ts]

-- checks the status of a library
libStatus :
     {auto _ : HasIO io}
  -> {auto e : Env}
  -> PkgName
  -> (p    : Package)
  -> (d    : Desc t)
  -> (deps : List (DPair Package PkgStatus))
  -> EitherT PackErr io (PkgStatus p)
libStatus n p d deps = do
  h    <- pkgHash
  True <- exists (pkgInstallDir n h p d) | False => do pure (Missing missingHash)
  b    <- exists $ pkgDocs n h p d
  pure (Installed h b)
  where
    mkHash : String -> Hash
    mkHash s =
      MkHash $ hashStrings $ s :: map (\(_ ** s) => value $ hash s) deps

    localHash : Hash
    localHash = mkHash nanoString

    missingHash : Hash
    missingHash =
      case p of
        Git _ c _ _ _ _          => mkHash c.value
        Core                   _ => MkHash e.db.idrisCommit.value
        Local {}                 => localHash

    pkgHash : EitherT PackErr io Hash
    pkgHash =
      case p of
        Local dir ipkg pkgPath _ => do
          let ts  := libTimestamp n
              dir := localSrcDir d
          True <- fileExists ts       | False => pure localHash
          ""   <- newerSrc ts dir     | s     => pure localHash
          ""   <- newerIpkg ts d.path | s     => pure localHash
          s    <- read ts
          debug "timestamp for \{n}: \{s}"
          debug "hash for \{n}: \{mkHash $ trim s}"
          pure (mkHash $ trim s)
        _ => pure missingHash

||| Generates the `AppStatus` of a package representing an application.
export
appStatus :
     {auto _ : HasIO io}
  -> {auto _ : Env}
  -> PkgName
  -> (h    : Hash)
  -> (p    : Package)
  -> (exe  : Body)
  -> EitherT PackErr io (AppStatus p)
appStatus n h p exe = do
  True <- fileExists (pkgExec n h p exe) | False => pure (Missing h)
  True <- fileExists (pathExec exe)      | False => pure (Installed h)
  pure (BinInstalled h)

loadIpkg :
     {auto _ : HasIO io}
  -> {auto e : Env}
  -> PkgName
  -> Package
  -> EitherT PackErr io (Desc U)
loadIpkg n (Git u c i _ _ _) =
  let cache  := ipkgCachePath n c i
      tmpLoc := gitTmpDir n </> i
   in parseIpkgFile cache tmpLoc
loadIpkg n (Local d i _ _)    = parseIpkgFile (d </> i) (d </> i)
loadIpkg n (Core c)           =
  let cache  := coreCachePath c
      tmpLoc := gitTmpDir compiler </> coreIpkgPath c
   in parseIpkgFile cache tmpLoc

||| Try to fully resolve a library given as a package name.
||| This will look up the library in the current package collection
||| and will fetch and read its (possibly cached) `.ipkg` file.
export
resolveLib :
     {auto _ : HasIO io}
  -> {auto e : Env}
  -> {default [<] vis : SnocList PkgName}
  -> PkgName
  -> EitherT PackErr io (ResolvedLib U)
resolveLib n = assert_total $ do
  checkCycle vis [n]
  Nothing <- lookupLib n | Just pkg => pure pkg
  case lookup n e.all of
    Nothing => throwE (UnknownPkg n)
    Just p  => do
      d    <- loadIpkg n p
      deps <- traverse resolveDep $ dependencies d
      lib  <- libStatus n p d deps
      cacheLib n $ RL p (hash lib) n d lib deps

  where
    checkCycle : SnocList PkgName -> List PkgName -> EitherT PackErr io ()
    checkCycle [<]       xs = pure ()
    checkCycle (sx :< x) xs =
      if x == n then throwE (CyclicDeps $ x::xs) else checkCycle sx (x::xs)

    resolveDep : PkgName -> EitherT PackErr io (DPair Package PkgStatus)
    resolveDep m =
      (\l => (_ ** l.status)) <$> resolveLib {vis = vis :< n} m

||| Try to fully resolve an application given as a package name.
||| This will look up the app in the current package collection
||| and will fetch and read its (possibly cached) `.ipkg` file.
export covering
resolveApp : HasIO io => Env => PkgName -> EitherT PackErr io (ResolvedApp U)
resolveApp n = do
  RL p h n d _ ds <- resolveLib n
  case exec d of
    Nothing  => throwE (NoApp n)
    Just exe => (\s => RA p h n d s exe ds) <$> appStatus n h p exe

||| Try to fully resolve an application or library given as a package.
||| This will look up the package name in the current package collection
||| and will fetch and read its (possibly cached) `.ipkg` file.
export covering
resolveAny :
     {auto _ : HasIO io}
  -> {auto _ : Env}
  -> InstallType
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
  putStrLn . interpolate $ pkgExec ra.name ra.hash ra.pkg ra.exec

--------------------------------------------------------------------------------
--          Deletable
--------------------------------------------------------------------------------

depString : (depsOfX : List PkgName) -> (x,y : PkgName) -> Maybe String
depString depsOfX x y =
  toMaybe (y `elem` depsOfX) "\{quote x} depends on \{quote y}"

consider : List PkgName -> ResolvedLib s -> Bool
consider ns x = isInstalled x && not (x.name `elem` ns)

printDeps :
     {auto _ : HasIO io}
  -> {auto _ : Env}
  -> List (ResolvedLib s)
  -> PkgName
  -> EitherT PackErr io Bool
printDeps rls n =
  case map nameStr (filter (elem n . dependencies) rls) of
    Nil  => pure False
    [n1] => warn "Package \{quote n1} depends on \{quote n}." $> True
    ns   => logMany Warning
              "The following packages depend on \{quote n}:" ns $> True

delPrompt : String
delPrompt = """
  Some installed packages depend on the ones about to be deleted.
  This might leave the package library in an inconsistent state.
  Continue (yes/no*)?
  """

||| Verifies that the given list of libraries is safe to be deleted
export covering
checkDeletable : HasIO io => (e : Env) => List PkgName -> EitherT PackErr io ()
checkDeletable ns = do
  ss <- filter (consider ns) <$> traverse resolveLib (keys e.all)
  bs <- traverse (printDeps ss) ns
  when (any id bs) $ confirmOrAbort Warning delPrompt

--------------------------------------------------------------------------------
--         Garbage Collection
--------------------------------------------------------------------------------

idrisDelDir : (e : Env) => Body -> Maybe (Path Abs)
idrisDelDir b =
  let s := interpolate b
   in toMaybe (s /= "pack" && all (\ic => s /= ic.value) e.config.allIdrisCommits) (installDir /> b)

packDelDir : (e : Env) => Body -> Maybe (Path Abs)
packDelDir b =
  let s := interpolate b
   in toMaybe (s /= V.version.value) (packParentDir /> b)

tmpDelDir : (e : Env) => Body -> Maybe (Path Abs)
tmpDelDir b =
  let s := interpolate b
      p := cacheDir /> b
   in toMaybe ((".tmp" `isPrefixOf` s) && p /= Types.tmpDir) p

purgeDirs : HasIO io => (e : Env) => EitherT PackErr io (List $ Path Abs)
purgeDirs = join <$> (entries commitDir >>= traverse purgePaths)
  where
    purgePaths : Body -> EitherT PackErr io (List $ Path Abs)
    purgePaths "idris2" = pure []
    purgePaths x        =
     let pkg := MkPkgName (interpolate x)
         dir := commitDir /> x
      in case lookup pkg e.all of
           Nothing => pure [dir]
           Just _  => do
             rl <- resolveLib pkg
             es <- entries (commitDir /> x)
             pure $ (dir />) <$> filter (\b => "\{b}" /= rl.hash.value) es

gcDirs : HasIO io => (e : Env) => EitherT PackErr io (List $ Path Abs)
gcDirs = do
  ds <- mapMaybe idrisDelDir <$> entries installDir
  ps <- mapMaybe packDelDir <$> entries packParentDir
  ts <- mapMaybe tmpDelDir <$> entries cacheDir
  pd <- if e.config.gcPurge then purgeDirs else pure []
  pure (ds ++ ps ++ ts ++ pd)

||| Delete installations from previous package collections.
export
garbageCollector : HasIO io => Env -> EitherT PackErr io ()
garbageCollector e = do
  all <- gcDirs
  when (e.config.gcPrompt && not (null all)) $ do
    let msg := "The following directories will be deleted."
    confirmManyOrAbort Warning msg (interpolate <$> all)
  when (null all) $ info "Nothing to clean up."
  for_ all rmDir

--------------------------------------------------------------------------------
--         Installation Plan
--------------------------------------------------------------------------------

pkgNeedsInstalling : {auto c : Config} -> PkgStatus p -> Bool
pkgNeedsInstalling (Missing h)         = True
pkgNeedsInstalling (Installed h True)  = False
pkgNeedsInstalling (Installed h False) = c.withDocs

appNeedsInstalling : (withWrapperScript : Bool) -> AppStatus p -> Bool
appNeedsInstalling _ (Missing h)      = True
appNeedsInstalling b (Installed h)    = b
appNeedsInstalling _ (BinInstalled h) = False

needsInstalling : {auto c : Config} -> LibOrApp t s -> Bool
needsInstalling (Lib x)   = pkgNeedsInstalling x.status
needsInstalling (App b x) = appNeedsInstalling b x.status

showPlan : List (InstallType, PkgName) -> String
showPlan = unlines . map (\(t,n) => "\{t} \{n}")

||| Resolve the (transitive) dependencies of the given libs and apps.
export covering
transitiveDeps :
     {auto _ : HasIO io}
  -> {auto _ : Env}
  -> List (InstallType, PkgName)
  -> EitherT PackErr io (List $ LibOrApp U U)
transitiveDeps ps = go empty Lin (map Right ps)
  where
    covering
    go :
         (planned  : SortedMap (InstallType, PkgName) ())
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

compilerApp : (InstallType, PkgName) -> Bool
compilerApp (App _, MkPkgName "idris2") = True
compilerApp _ = False

||| Create a build plan for the given list of packages and apps
||| plus their dependencies.
|||
||| All packages depend on the prelude and
||| base, so we make sure these are installed as well.
export covering
plan :
     {auto _ : HasIO io}
  -> {auto _ : Env}
  -> List (InstallType, PkgName)
  -> EitherT PackErr io (List SafePkg)
plan ps =
  let neededPs := filter (not . compilerApp) ps
  -- ^ we never need the compiler app because it is installed as part of the
  -- requisite environment to run any Install command.
      ps' := (Library, "prelude") :: (Library, "base") :: neededPs
   in do
     debug "Building plan for the following libraries: \n\{showPlan ps}"
     loas <- filter needsInstalling <$> transitiveDeps ps'
     traverse checkLOA loas
