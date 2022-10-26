module Pack.Runner.Install

import Core.FC
import Data.IORef
import Data.Maybe
import Idris.Package.Types
import Pack.Config
import Pack.Core
import Pack.Database
import Pack.Runner.Database
import System.Escape

%default total

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

export
ipkgCodeGen : (e : Env) => PkgDesc -> Codegen
ipkgCodeGen desc = case e.config.codegen of
  Default => getCG (maybe [] (filter (not . null) . words . snd) desc.options)
  cg      => cg

  where getCG : List String -> Codegen
        getCG ("--cg"      :: cg :: _) = fromString cg
        getCG ("--codegen" :: cg :: _) = fromString cg
        getCG [_]                      = Default
        getCG []                       = Default
        getCG (h :: t)                 = getCG t

coreGitDir : (e : Env) => Path Abs
coreGitDir = gitTmpDir compiler

copyApp : HasIO io => IdrisEnv => SafeApp -> EitherT PackErr io ()
copyApp ra =
  let dir := pkgBinDir ra.name ra.pkg
   in do
     debug "Copying application to \{dir}"
     mkDir dir
     sys ["cp", "-r", Escapable "\{buildPath ra.desc}/exec/" ++ NoEscape "*", dir]

pthStr : PackDir => Bool -> String
pthStr False = ""
pthStr True = """
  export IDRIS2_PACKAGE_PATH="$(\{packExec} package-path)"
  export IDRIS2_LIBS="$(\{packExec} libs-path)"
  export IDRIS2_DATA="$(\{packExec} data-path)"
  """

-- When linking to a binary from pack's `bin` directory,
-- we distinguish between applications,
-- which need acceess to the Idris package path and those,
-- which don't. For the former, we create a wrapper script
-- where we first set the `IDRIS2_PACKAGE_PATH` variable
-- before invoking the binary. For both cases, we let pack
-- decide which version to use.
appLink :  HasIO io
        => (e: Env)
        => PackDir
        => (exec        : Body)
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

      APPLICATION="$(\{packExec} app-path \{app})"
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
libPkg :  HasIO io
       => (e : IdrisEnv)
       => (env        : List (String,String))
       -> (cleanBuild : Bool)
       -> (cmd        : CmdArgList)
       -> (desc       : Desc Safe)
       -> EitherT PackErr io ()
libPkg env cleanBuild cmd desc =
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

     inDir (desc.path.parent) (\_ => sysWithEnvAndLog Build s pre)

--------------------------------------------------------------------------------
--          Installing Idris
--------------------------------------------------------------------------------

||| Builds and installs the Idris commit given in the environment.
export covering
mkIdris : HasIO io => (e : Env) => EitherT PackErr io IdrisEnv
mkIdris = do
  debug "Checking Idris installation"
  when !(missing idrisInstallDir) $ do
    debug "No Idris compiler found. Installing..."
    withCoreGit $ \dir => do
      case e.config.bootstrap of
        True  =>
          sysAndLog Build ["make", "bootstrap", prefixVar, schemeVar]
        False =>
          sysAndLog Build ["make", "support", prefixVar, schemeVar] >>
          sysAndLog Build ["make", "idris2-exec", prefixVar, schemeVar]

      sysAndLog Build ["make", "install-support", prefixVar]
      sysAndLog Build ["make", "install-idris2", prefixVar]
      sysAndLog Build ["make", "clean-libs"]
      sysAndLog Build ["rm", "-r", "build/ttc", "build/exec"]
      cacheCoreIpkgFiles dir

  appLink "idris2" "idris2" True Default
  pure $ MkIdrisEnv %search ItHasIdris

--------------------------------------------------------------------------------
--          Installing Libs
--------------------------------------------------------------------------------

withSrcStr : (c : Config) => String
withSrcStr = case c.withSrc of
  True  => " (with sources)"
  False => ""

installImpl :  HasIO io
            => (e : IdrisEnv)
            => (dir : Path Abs)
            -> SafeLib
            -> EitherT PackErr io ()
installImpl dir rl =
  let pre := libInstallPrefix rl
      cmd := installCmd e.env.config.withSrc
   in do
     info "Installing library\{withSrcStr}: \{name rl}"
     libPkg pre True cmd rl.desc
     when e.env.config.withDocs $ libPkg pre False ["--mkdoc"] rl.desc
     when !(exists $ dir /> "lib") $
       copyDir (dir /> "lib") (pkgLibDir rl.name rl.pkg)

||| Install the given resolved library.
export
installLib :  HasIO io => IdrisEnv => SafeLib -> EitherT PackErr io ()
installLib rl = case rl.status of
  Installed => pure ()
  _         => withPkgEnv rl.name rl.pkg $ \dir =>
    let ipkgAbs := ipkg dir rl.pkg
     in case rl.pkg of
          GitHub u c ipkg _ => do
            let cache := ipkgCachePath rl.name c ipkg
            copyFile cache ipkgAbs
            installImpl dir rl

          Local _ _ _ => do
            installImpl dir rl
            write (libTimestamp rl.name rl.pkg) ""

          Core c => do
            let cache   := coreCachePath c
            copyFile cache ipkgAbs
            case c of
              IdrisApi =>
                sysAndLog Build ["make", "src/IdrisPaths.idr", prefixVar]
              _        => pure ()
            installImpl dir rl

--------------------------------------------------------------------------------
--          Installing Apps
--------------------------------------------------------------------------------

||| Install an Idris application given as a package name
||| TODO: Install wrapper script only if necessary
export covering
installApp :  HasIO io
           => IdrisEnv
           => (withWrapperScript : Bool)
           -> SafeApp
           -> EitherT PackErr io ()
installApp b ra =
  let cg := ipkgCodeGen ra.desc.desc
  in case ra.status of
    BinInstalled => pure ()
    Installed    => case b of
      False => pure ()
      True  => appLink ra.exec ra.name (usePackagePath ra) cg
    _            => withPkgEnv ra.name ra.pkg $ \dir =>
      let ipkgAbs := ipkg dir ra.pkg
       in case ra.pkg of
            Core _            => pure ()
            GitHub u c ipkg b => do
              let cache   := ipkgCachePath ra.name c ipkg
              copyFile cache ipkgAbs
              libPkg [] True ["--build"] (notPackIsSafe ra.desc)
              copyApp ra
              when b $ appLink ra.exec ra.name (usePackagePath ra) cg
            Local _ _ b    => do
              libPkg [] True ["--build"] (notPackIsSafe ra.desc)
              copyApp ra
              when b $ appLink ra.exec ra.name (usePackagePath ra) cg
              write (appTimestamp ra.name ra.pkg) ""


||| Install the given resolved library or application.
export covering
installAny :  HasIO io
           => IdrisEnv
           => SafePkg
           -> EitherT PackErr io ()
installAny (Lib sli)   = installLib sli
installAny (App b sla) = installApp b sla

--------------------------------------------------------------------------------
--          Generating API Docs
--------------------------------------------------------------------------------

covering
docsImpl :  HasIO io
         => (e : IdrisEnv)
         => SafeLib
         -> EitherT PackErr io ()
docsImpl rl = do
  let docsDir : Path Abs
      docsDir = buildPath rl.desc /> "docs"

      htmlDir : Path Abs
      htmlDir = docsDir /> "docs"

  when e.env.config.useKatla $ do
    info "Building source docs for: \{name rl}"
    rp <- resolveApp "katla"
    let katla := pkgExec rp.name rp.pkg rp.exec
    fs <- map (MkF htmlDir) <$> htmlFiles htmlDir
    for_ fs $ \htmlFile =>
      let Just ds@(MkDS _ src ttm srcHtml) := sourceForDoc rl.desc htmlFile
            | Nothing => pure ()
       in sysAndLog Build [katla, "html", src, ttm, NoEscape ">", srcHtml] >>
          insertSources ds

  let docs := pkgDocs rl.name rl.pkg
  when !(exists docs) (rmDir docs)
  copyDir docsDir docs

||| Install the API docs for the given resolved library.
export covering
installDocs :  HasIO io
            => IdrisEnv
            => SafeLib
            -> EitherT PackErr io ()
installDocs rl = do
  withPkgEnv rl.name rl.pkg $ \dir => docsImpl rl

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
install :  HasIO io
        => (e : IdrisEnv)
        => List (InstallType, PkgName)
        -> EitherT PackErr io ()
install ps = do
  all <- plan $ katla <+> autoPairs <+> ps
  logMany Info "Installing libraries:" (libInfo all)
  logMany Info "Installing apps:" (appInfo all)
  for_ all installAny

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
installDeps :  HasIO io
            => IdrisEnv
            => Desc Safe
            -> EitherT PackErr io ()
installDeps = install . map (Library,) . dependencies

||| Creates a packaging environment with Idris2 installed.
export covering
idrisEnv :  HasIO io
         => PackDir
         => TmpDir
         => LibCache
         => LineBufferingCmd
         => MetaConfig
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

     withGit "pack" packRepo commit $ \dir => do
       let ipkg := MkF dir "pack.ipkg"
       d <- parseLibIpkg ipkg ipkg
       installDeps d
       let installDir    := packInstallDir commit
           installedExec := installDir /> "pack"
       ex <- exists installedExec
       case ex of
         True  => link installedExec packExec
         False => do
           libPkg [] True ["--build"] d
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
  rmDir (pkgBinDir ra.name ra.pkg)

covering
removeLib : HasIO io => Env => PkgName -> EitherT PackErr io ()
removeLib n = do
  rl <- resolveLib n
  case isInstalled rl of
    True  => info "Removing library \{n}" >>
             rmDir (pkgInstallDir rl.name rl.pkg rl.desc)
    False => warn "Package \{n} is not installed. Ignoring."

||| Remove the given libs or apps
export covering
remove : HasIO io => Env => List (PkgType,PkgName) -> EitherT PackErr io ()
remove ps = do
  ref <- emptyCache
  for_ ps  $ \case (Lib,n) => removeLib n
                   (Bin,n) => removeApp n

||| Remove the given libs
export covering
removeLibs : HasIO io => Env => List PkgName -> EitherT PackErr io ()
removeLibs ns = do
  checkDeletable ns
  remove $ map (Lib,) ns

||| Remove the given apps
export covering
removeApps : HasIO io => Env => List PkgName -> EitherT PackErr io ()
removeApps = remove . map (Bin,)
