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
coreGitDir = gitDir tmpDir compiler e.db.idrisCommit

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

||| Use the installed Idris to run an operation on
||| a library `.ipkg` file.
export
libPkg :  HasIO io
       => IdrisEnv
       => (env  : List (String,String))
       -> (cmd  : CmdArgList)
       -> (desc : Desc Safe)
       -> EitherT PackErr io ()
libPkg env cmd desc =
  let exe := idrisWithCG
      s   := exe ++ cmd ++ [desc.path.file]
   in do
     pre <- (env ++) <$> buildEnv
     debug "About to run: \{escapeCmd s}"
     inDir (desc.path.parent) (\_ => sysWithEnvAndLog Build s pre)

--------------------------------------------------------------------------------
--          Installing Idris
--------------------------------------------------------------------------------

||| Builds and installs the Idris commit given in the environment.
export covering
mkIdris : HasIO io => Env => EitherT PackErr io IdrisEnv
mkIdris = do
  debug "Checking Idris installation"
  when !(missing idrisInstallDir) $ do
    debug "No Idris compiler found. Installing..."
    withCoreGit $ \dir => do
      sysAndLog Build ["make", "bootstrap", prefixVar, schemeVar]
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
     libPkg pre cmd rl.desc
     when e.env.config.withDocs $ libPkg pre ["--mkdoc"] rl.desc
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
              IdrisApi => sysAndLog Build ["make", "src/IdrisPaths.idr"]
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
              libPkg [] ["--build"] (notPackIsSafe ra.desc)
              copyApp ra
              when b $ appLink ra.exec ra.name (usePackagePath ra) cg
            Local _ _ b    => do
              libPkg [] ["--build"] (notPackIsSafe ra.desc)
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
autoPairs = map (Library,) c.autoLibs ++
            map (App True,) c.autoApps

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
         => MetaConfig
         -> (fetch : Bool)
         -> EitherT PackErr io IdrisEnv
idrisEnv mc fetch = env mc fetch >>= (\e => mkIdris)

||| Update the pack installation
export covering
update : HasIO io => IdrisEnv -> EitherT PackErr io ()
update e =
  let dir  := packClone
      ipkg := MkF dir "pack.ipkg"
      bin  := packBinDir
   in finally (rmDir dir) $ do
        info "Updating pack. If this fails, try switching to the latest package collection."
        gitClone packRepo dir
        traverse_ (\c => inDir dir $ \_ => gitCheckout c) packCommit

        d <- parseLibIpkg ipkg ipkg
        installDeps d
        inDir dir $ \_ => do
          vers <- MkCommit <$> sysRun ["git", "rev-parse", "HEAD"]
          let installDir    := packInstallDir vers
              installedExec := installDir /> "pack"
          ex <- exists installedExec
          case ex of
            True  => link installedExec packExec
            False => do
              libPkg [] ["--build"] d
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
  info "Removing library \{n}"
  rmDir (pkgInstallDir rl.name rl.pkg rl.desc)

||| Remove a library or application.
export covering
remove : HasIO io => Env => List (PkgType,PkgName) -> EitherT PackErr io ()
remove ps = do
  ref <- emptyCache
  for_ ps  $ \case (Lib,n) => removeLib n
                   (Bin,n) => removeApp n
