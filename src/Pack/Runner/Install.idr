module Pack.Runner.Install

import Core.FC
import Data.IORef
import Data.Maybe
import Data.SortedMap
import Idris.Package.Types
import Pack.Config
import Pack.Core
import Pack.Database
import Pack.Runner.Database
import System.Escape

%default total

--------------------------------------------------------------------------------
--          Paths
--------------------------------------------------------------------------------

pathDirs :
     {auto _ : HasIO io}
  -> {auto _ : PackDirs}
  -> {auto e : Env}
  -> (pre : String)
  -> (pth : PkgName -> Hash -> Package -> Path Abs)
  -> EitherT PackErr io String
pathDirs pre pth = do
  rs <- traverse resolveLib (keys e.all)
  ps <- filterM (\r => exists $ pth r.name r.hash r.pkg) rs
  let ps' := filter (not . isCorePkg . value . name) ps
  pure $
      fastConcat
    . intersperse ":"
    . (pre ::)
    $ map (\r => "\{pth r.name r.hash r.pkg}") ps'

||| Directories to be listed in the `IDRIS2_PACKAGE_PATH` variable, so
||| that Idris finds all libraries installed by pack in custom locations.
export
packagePathDirs : HasIO io => Env -> EitherT PackErr io String
packagePathDirs _ = pathDirs "\{idrisInstallDir}" pkgPathDir

||| Directories to be listed in the `IDRIS2_LIBS` variable, so
||| that Idris finds all `.so` files installed by pack in custom locations.
export
packageLibDirs : HasIO io => Env -> EitherT PackErr io String
packageLibDirs _ = pathDirs "\{idrisLibDir}" pkgLibDir

||| Directories to be listed in the `IDRIS2_DATA` variable, so
||| that Idris finds all support files installed by pack in custom locations.
export
packageDataDirs : HasIO io => Env -> EitherT PackErr io String
packageDataDirs _ = pathDirs "\{idrisDataDir}" pkgDataDir

||| `IDRIS2_PACKAGE_PATH` variable to be used with Idris, so
||| that it finds all libraries installed by pack in custom locations.
export
packagePath : HasIO io => Env => EitherT PackErr io (String, String)
packagePath =
  ("IDRIS2_PACKAGE_PATH",) <$>  packagePathDirs %search

||| `IDRIS2_LIBS` variable to be used with Idris, so
||| that it finds all `.so` files installed by pack in custom locations.
export
libPath : HasIO io => Env => EitherT PackErr io (String, String)
libPath = ("IDRIS2_LIBS",) <$> packageLibDirs %search

||| `IDRIS2_DATA` variable to be used with Idris, so
||| that it finds all support files installed by pack in custom locations.
export
dataPath : HasIO io => Env => EitherT PackErr io (String, String)
dataPath = ("IDRIS2_DATA",) <$> packageDataDirs %search

||| This unifies `packagePath`, `libPath` and `dataPath`,
||| to generate an environment necessary to build packages with Idris
||| the dependencies of which are handled by pack.
export
buildEnv : HasIO io => Env => EitherT PackErr io (List (String,String))
buildEnv =
  let pre := if useRacket then [("IDRIS2_CG", "racket")] else []
   in (pre ++ ) <$> sequence [packagePath, libPath, dataPath]

||| Idris executable loading the given package plus the
||| environment variables needed to run it.
export
idrisWithPkg :
     {auto _ : HasIO io}
  -> {auto _ : IdrisEnv}
  -> ResolvedLib t
  -> EitherT PackErr io (CmdArgList, List (String,String))
idrisWithPkg rl =
  (idrisWithCG ++ ["-p", name rl],) <$> buildEnv

||| Idris executable loading the given packages plus the
||| environment variables needed to run it.
export
idrisWithPkgs :
     {auto _ : HasIO io}
  -> {auto _ : IdrisEnv}
  -> List (ResolvedLib t)
  -> EitherT PackErr io (CmdArgList, List (String,String))
idrisWithPkgs [] = pure (idrisWithCG, [])
idrisWithPkgs pkgs =
  let ps = concatMap (\p => ["-p", name p]) pkgs
   in (idrisWithCG ++ ps,) <$> buildEnv

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

export
ipkgCodeGen : (e : Env) => PkgDesc -> Codegen
ipkgCodeGen desc = case e.config.codegen of
  Default => getCG (maybe [] (filter (not . null) . words . snd) desc.options)
  cg      => cg

  where
    getCG : List String -> Codegen
    getCG ("--cg"      :: cg :: _) = fromString cg
    getCG ("--codegen" :: cg :: _) = fromString cg
    getCG [_]                      = Default
    getCG []                       = Default
    getCG (h :: t)                 = getCG t

coreGitDir : (e : Env) => Path Abs
coreGitDir = gitTmpDir compiler

copyApp : HasIO io => IdrisEnv => SafeApp -> EitherT PackErr io ()
copyApp ra =
  let dir := pkgBinDir ra.name ra.hash ra.pkg
   in do
     debug "Copying application to \{dir}"
     mkDir dir
     sys ["cp", "-r", Escapable "\{buildPath ra.desc}/exec/" ++ NoEscape "*", dir]

noAppError : (app : PkgName) -> List String
noAppError app = lines $ """
  [ fatal ] Package `\{app}` is not built or not installed in the current
            environment. Maybe, it was installed with an older compiler version
            or using a local `pack.toml` which is not available in the current
            directory. Try to reinstall it with `pack install-app \{app}`.
  """

pthStr : (c : Config) => PackDirs => Bool -> String
pthStr False = ""
pthStr True =
  let racket := if useRacket then "export \{schemeVar}" else ""
   in """
   export IDRIS2_PACKAGE_PATH="$(\{packExec} package-path)"
   export IDRIS2_LIBS="$(\{packExec} libs-path)"
   export IDRIS2_DATA="$(\{packExec} data-path)"
   \{racket}
   """

-- When linking to a binary from pack's `bin` directory,
-- we distinguish between applications,
-- which need acceess to the Idris package path and those,
-- which don't. For the former, we create a wrapper script
-- where we first set the `IDRIS2_PACKAGE_PATH` variable
-- before invoking the binary. For both cases, we let pack
-- decide which version to use.
appLink :
     {auto _ : HasIO io}
  -> {auto e : Env}
  -> {auto _ : PackDirs}
  -> (exec        : Body)
  -> (app         : PkgName)
  -> (withPkgPath : Bool)
  -> (codeGen     : Codegen)
  -> EitherT PackErr io ()
appLink exec app withPkgPath cg =
  let
      interp  := case cg of
        Node => "node "
        _ => ""
      target  := MkF packBinDir exec
      content := """
      #!/bin/sh

      if ! APPLICATION="$(\{packExec} app-path \{app})" || [ ! -r "$APPLICATION" ]; then {
      \{unlines $ noAppError app <&> \s => "  echo '\{s}'"}
        } >&2; exit 2
      fi
      \{pthStr withPkgPath}

      \{interp}$APPLICATION "$@"
      """
   in write target content >> sys ["chmod", "+x", target]

installCmd : (withSrc : Bool) -> CmdArgList
installCmd True  = ["--install-with-src"]
installCmd False = ["--install"]

||| Check if the build directory associated with the given `.ipkg` file
||| is outdated, either because it does not contain an `.idrisVersion` file,
||| or because this file has a different commit tag than the Idris version
||| we use currently.
export covering
checkBuildDir :  HasIO io => (e : IdrisEnv) => Desc Safe -> EitherT PackErr io ()
checkBuildDir d =
  let buildDir := buildPath d
      version  := the (File Abs) (buildDir /> ".idrisCommit")
      commit   := e.env.db.idrisCommit.value
   in do
     str <- readIfExists version ""
     when (str /= commit) $ do
       rmDir buildDir
       write version commit

dependsMsg : Path Abs -> String
dependsMsg p = """
  Found local package directory at \{p}.
  Using local package directories together with pack is highly discouraged,
  as they might interfere with the packages managed by pack in an unpredictable
  manner.
  """

||| Use the installed Idris to run an operation on
||| a library `.ipkg` file.
export covering
libPkg :
     {auto _     : HasIO io}
  -> {auto e     : IdrisEnv}
  -> (env        : List (String,String))
  -> (logLevel   : LogLevel)
  -> (cleanBuild : Bool)
  -> (cmd        : CmdArgList)
  -> (desc       : Desc Safe)
  -> EitherT PackErr io ()
libPkg env lvl cleanBuild cmd desc =
  let exe := idrisWithCG
      s   := exe ++ cmd ++ [desc.path.file]
   in do
     pre <- (env ++) <$> buildEnv
     debug "About to run: \{escapeCmd s}"
     when cleanBuild (checkBuildDir desc)

     -- warn if we find a `depends` directory in a local package
     let dependsDir := desc.path.parent /> "depends"
     when !(exists dependsDir) $
       when e.env.config.warnDepends $ warn (dependsMsg dependsDir)

     inDir (desc.path.parent) (\_ => sysWithEnvAndLog lvl s pre)

--------------------------------------------------------------------------------
--          Installing Idris
--------------------------------------------------------------------------------

hasTTC : String -> Bool
hasTTC = any (("--ttc-version" `isPrefixOf`) . trim) . lines

covering
getTTCVersion : HasIO io => Env => EitherT PackErr io TTCVersion
getTTCVersion = do
  hlp <- sysRun [idrisExec, "--help"]
  case hasTTC hlp of
    True  => do
      str <- sysRun [idrisExec, "--ttc-version"]
      case Body.parse (trim str) of
        Just v  => debug "Using TTC version \{v}" $> TTCV (Just v)
        Nothing => warn "Failed to parse TTC version \{str}" $> TTCV Nothing
    False => debug "No TTC version given by Idris" $> TTCV Nothing

-- Tries to build Idris from an existing version of the compiler.
tryDirectBuild : HasIO io => Env => io (Either PackErr ())
tryDirectBuild =
  runEitherT $ do
    sysAndLog Build ["make", "support"]
    sysAndLog Build ["make", "idris2-exec", schemeVar]

idrisCleanup : HasIO io => Env => io ()
idrisCleanup =
  ignoreError $ do
    sysAndLog Build ["make", "clean-libs"]
    sysAndLog Build ["rm", "-r", "build/ttc", "build/exec"]

idrisBootstrapStage3 : HasIO io => (e : Env) => Path Abs -> EitherT PackErr io ()
idrisBootstrapStage3 dir = do
  let prefVar = mkPrefixVar dir
  debug "Install bootstrapped Idris..."
  sysAndLog Build ["make", "bootstrap-install", prefVar, schemeVar]
  idrisCleanup

  debug "Stage 3: Rebuilding Idris..."
  let idrisBootVar = mkIdrisBootVar $ dir /> "bin" /> "idris2"
  let idrisDataVar = mkIdrisDataVar $ dir /> idrisDir /> "support"
  sysAndLog Build ["make", "idris2-exec", idrisBootVar, idrisDataVar, schemeVar]

  ignoreError $ sysAndLog Build ["rm", "-rf", dir]

idrisBootstrap : HasIO io => (e : Env) => Path Abs -> EitherT PackErr io ()
idrisBootstrap dir = do
  debug "Bootstrapping Idris..."
  sysAndLog Build ["make", bootstrapCmd, schemeVar]
  when e.config.bootstrapStage3 $ do
    idrisBootstrapStage3 $ dir </> "bootstrapped"
  ignoreError $ sysAndLog Build ["make", "bootstrap-clean"]

||| Builds and installs the Idris commit given in the environment.
export covering
mkIdris : HasIO io => (e : Env) => EitherT PackErr io IdrisEnv
mkIdris = do
  debug "Checking Idris installation"
  when !(missing idrisInstallDir) $ do
    debug "No Idris compiler found. Installing..."
    withCoreGit $ \dir => do
      case e.config.bootstrap of
        True  => idrisBootstrap dir
        False =>
          -- if building with an existing installation fails for whatever reason
          -- we revert to bootstrapping
          tryDirectBuild >>= \case
            Left x => do
              warn "Building Idris failed. Trying to bootstrap now."
              idrisBootstrap dir
            Right () => pure ()

      sysAndLog Build ["make", "install-support", prefixVar]
      sysAndLog Build ["make", "install-idris2", prefixVar]
      idrisCleanup
      cacheCoreIpkgFiles dir

  appLink "idris2" "idris2" True Default
  ttc <- getTTCVersion
  pure $ MkIdrisEnv %search ttc ItHasIdris

--------------------------------------------------------------------------------
--          Installing Libs
--------------------------------------------------------------------------------

withSrcStr : (c : Config) => String
withSrcStr = case c.withSrc of
  True  => " (with sources)"
  False => ""

maybeGiveNotice : HasIO io => Config => SafeLib -> io ()
maybeGiveNotice (RL (Git _ _ _ _ _ (Just notice)) _ _ _ _ _) = warn notice
maybeGiveNotice _ = pure ()

installImpl :
     {auto _ : HasIO io}
  -> {auto e : IdrisEnv}
  -> (dir : Path Abs)
  -> SafeLib
  -> EitherT PackErr io ()
installImpl dir rl =
  let pre      := libInstallPrefix rl
      instCmd  := installCmd e.env.config.withSrc
      libDir   := rl.desc.path.parent </> "lib"
   in do
     info "Installing library\{withSrcStr}: \{name rl}"
     maybeGiveNotice rl
     when (isInstalled rl) $ do
       info "Removing currently installed version of \{name rl}"
       rmDir (pkgInstallDir rl.name rl.hash rl.pkg rl.desc)
       rmDir (pkgLibDir rl.name rl.hash rl.pkg)
     libPkg pre Build True ["--build"] rl.desc
     libPkg pre Debug False instCmd rl.desc
     debug "checking if libdir at \{libDir} exists"
     when !(exists libDir) $ do
       debug "copying lib dir"
       copyDir libDir (pkgLibDir rl.name rl.hash rl.pkg)

preInstall :
     {auto _ : HasIO io}
  -> {auto e : IdrisEnv}
  -> SafeLib
  -> EitherT PackErr io ()
preInstall rl = withPkgEnv rl.name rl.pkg $ \dir =>
  let ipkgAbs := ipkg dir rl.pkg
   in case rl.pkg of
        Git u c ipkg _ _ _ => do
          let cache := ipkgCachePath rl.name c ipkg
          copyFile cache ipkgAbs
        Local _ _ _ _ => pure ()

        Core c => do
          let cache   := coreCachePath c
          copyFile cache ipkgAbs
          case c of
            IdrisApi =>
              sysAndLog Build ["make", "src/IdrisPaths.idr", prefixVar]
            _        => pure ()

-- Install the given resolved library.
installLib :
     {auto _ : HasIO io}
  -> {auto e : IdrisEnv}
  -> SafeLib
  -> EitherT PackErr io ()
installLib rl = case rl.status of
  Installed _ _ => pure ()
  _             => do
    preInstall rl
    withPkgEnv rl.name rl.pkg $ \dir => do
      installImpl dir rl
      case rl.pkg of
        Local _ _ _ _ =>
          when (not $ isInstalled rl) $ do
            debug "writing \{nanoString} to \{libTimestamp rl.name}"
            write (libTimestamp rl.name) nanoString
        _             => pure ()

    uncacheLib (name rl)

--------------------------------------------------------------------------------
--          Installing Apps
--------------------------------------------------------------------------------

-- Install an Idris application given as a package name
-- TODO: Install wrapper script only if necessary
covering
installApp :
     {auto _ : HasIO io}
  -> {auto e : IdrisEnv}
  -> (withWrapperScript : Bool)
  -> SafeApp
  -> EitherT PackErr io ()
installApp b ra =
  let cg := ipkgCodeGen ra.desc.desc
  in case ra.status of
    BinInstalled _ => pure ()
    Installed    _ => case b of
      False => pure ()
      True  => appLink ra.exec ra.name (usePackagePath ra) cg
    _            => withPkgEnv ra.name ra.pkg $ \dir =>
      let ipkgAbs := ipkg dir ra.pkg
       in case ra.pkg of
            Core _            => pure ()
            Git u c ipkg pp _ _ => do
              let cache   := ipkgCachePath ra.name c ipkg
              copyFile cache ipkgAbs
              libPkg [] Build True ["--build"] (notPackIsSafe ra.desc)
              copyApp ra
              when b $ appLink ra.exec ra.name pp cg
            Local _ _ pp _    => do
              libPkg [] Build True ["--build"] (notPackIsSafe ra.desc)
              copyApp ra
              when b $ appLink ra.exec ra.name pp cg
              write (libTimestamp ra.name) nanoString


--------------------------------------------------------------------------------
--          Generating API Docs
--------------------------------------------------------------------------------

covering
-- Install the API docs for the given resolved library.
installDocs :
     {auto _ : HasIO io}
  -> {auto e : IdrisEnv}
  -> SafeLib
  -> EitherT PackErr io ()
installDocs rl = case rl.status of
  Installed _ True => pure ()
  _                => withPkgEnv rl.name rl.pkg $ \dir => do
    let docsDir : Path Abs
        docsDir = buildPath rl.desc /> "docs"

        pre : List (String,String)
        pre = libInstallPrefix rl

        htmlDir : Path Abs
        htmlDir = docsDir /> "docs"

    info "Building source docs for: \{name rl}"
    preInstall rl
    libPkg pre Build False ["--mkdoc"] rl.desc

    when e.env.config.useKatla $ do
      info "Building highlighted sources for: \{name rl}"
      mkDir htmlDir
      rp <- resolveApp "katla"
      let katla := pkgExec rp.name rp.hash rp.pkg rp.exec
      fs <- map (MkF htmlDir) <$> htmlFiles htmlDir
      for_ fs $ \htmlFile =>
        let Just ds@(MkDS _ src ttm srcHtml) := sourceForDoc rl.desc htmlFile
              | Nothing => pure ()
         in when !(srcExists ds) $ do
              sysAndLog Build [katla, "html", src, ttm, NoEscape ">", srcHtml]
              insertSources ds

    let docs := pkgDocs rl.name rl.hash rl.pkg rl.desc
    when !(exists docs) (rmDir docs)
    copyDir docsDir docs
    uncacheLib (name rl)

katla : (c : Config) => List (InstallType, PkgName)
katla = if c.withDocs && c.useKatla then [(App False, "katla")] else []

autoPairs : (c : Config) => List (InstallType, PkgName)
autoPairs =
     map ((Library,) . corePkgName) [ Prelude, Base, Network ]
  ++ map (Library,) c.autoLibs
  ++ map (App True,) c.autoApps

libInfo : List SafePkg -> List String
libInfo = mapMaybe $ \case Lib rl  => Just "\{rl.name}"
                           App _ _ => Nothing

appInfo : List SafePkg -> List String
appInfo = mapMaybe $ \case App _ ra => Just "\{ra.name}"
                           Lib _    => Nothing

||| Install the given list of libraries or applications, by first
||| resolving each of them and then creating a build plan including
||| all dependencies of the lot.
export covering
install :
     {auto _ : HasIO io}
  -> {auto e : IdrisEnv}
  -> List (InstallType, PkgName)
  -> EitherT PackErr io ()
install ps = do
  all <- plan $ katla <+> autoPairs <+> ps
  logMany Info "Installing libraries:" (libInfo all)
  logMany Info "Installing apps:" (appInfo all)
  for_ all $ \case Lib rl   => installLib rl
                   App b rl => installApp b rl

  when e.env.config.withDocs $
    for_ all $ \case Lib rl  => installDocs rl
                     App _ _ => pure ()

||| Install the given list of libraries, by first
||| resolving each of them and then creating a build plan including
||| all dependencies of the lot.
export covering %inline
installLibs : HasIO io => IdrisEnv => List PkgName -> EitherT PackErr io ()
installLibs = install . map (Library,)

||| Install the given list of applications, by first
||| resolving each of them and then creating a build plan including
||| all dependencies of the lot.
export covering %inline
installApps : HasIO io => IdrisEnv => List PkgName -> EitherT PackErr io ()
installApps = install . map (App True,)

||| Install the (possibly transitive) dependencies of the given
||| loaded `.ipkg` file.
export covering
installDeps :
     {auto _ : HasIO io}
  -> {auto _ : IdrisEnv}
  -> Desc Safe
  -> EitherT PackErr io ()
installDeps = install . map (Library,) . dependencies

||| Creates a packaging environment with Idris2 installed.
export covering
idrisEnv :
     {auto _ : HasIO io}
  -> {auto _ : PackDirs}
  -> {auto _ : TmpDir}
  -> {auto _ : LibCache}
  -> {auto _ : LineBufferingCmd}
  -> MetaConfig
  -> (fetch : Bool)
  -> EitherT PackErr io IdrisEnv
idrisEnv mc fetch = env mc fetch >>= (\e => mkIdris)

||| Update the pack installation
export covering
update : HasIO io => IdrisEnv -> EitherT PackErr io ()
update e =
  let bin  := packBinDir
   in do
     info "Updating pack. If this fails, try switching to the latest package collection."
     commit <- maybe (gitLatest packRepo "main") pure packCommit
     info "Using commit \{commit}"

     withGit "pack" packRepo commit True $ \dir => do
       let ipkg := MkF dir "pack.ipkg"
       d <- parseLibIpkg ipkg ipkg
       installDeps d
       let installDir    := packInstallDir commit
           installedExec := installDir /> "pack"
       ex <- exists installedExec
       case ex of
         True  => link installedExec packExec
         False => do
           libPkg [] Build True ["--build"] d
           mkDir installDir
           sys ["cp", "-r", NoEscape "build/exec/*", installDir]
           link installedExec packExec

--------------------------------------------------------------------------------
--          Removing Libs
--------------------------------------------------------------------------------

covering
removeApp : HasIO io => Env => PkgName -> EitherT PackErr io ()
removeApp n = do
  info "Removing application \{n}"
  ra <- resolveApp n
  rmFile (pathExec ra.exec)
  rmDir (pkgBinDir ra.name ra.hash ra.pkg)

covering
removeLib : HasIO io => Env => PkgName -> EitherT PackErr io ()
removeLib n = do
  rl <- resolveLib n
  case isInstalled rl of
    True  => do
      info "Removing library \{n}"
      rmDir (pkgInstallDir rl.name rl.hash rl.pkg rl.desc)
      rmDir (pkgLibDir rl.name rl.hash rl.pkg)
    False => warn "Package \{n} is not installed. Ignoring."

||| Remove the given libs or apps
export covering
remove : HasIO io => Env => List (PkgType,PkgName) -> EitherT PackErr io ()
remove ps = do
  ref <- emptyCache
  for_ ps  $ \case (PLib,n) => removeLib n
                   (PApp,n) => removeApp n

||| Remove the given libs
export covering
removeLibs : HasIO io => Env => List PkgName -> EitherT PackErr io ()
removeLibs ns = do
  checkDeletable ns
  remove $ map (PLib,) ns

||| Remove the given apps
export covering
removeApps : HasIO io => Env => List PkgName -> EitherT PackErr io ()
removeApps = remove . map (PApp,)
