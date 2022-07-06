module Pack.Runner.Install

import Core.FC
import Data.Maybe
import Idris.Package.Types
import Pack.Config.Env
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types
import Pack.Runner.Database

%default total

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

coreGitDir : Env e -> Path Abs
coreGitDir e = gitDir (tmpDir e) compiler e.db.idrisCommit

copyApp : HasIO io => Env HasIdris -> SafeApp -> EitherT PackErr io ()
copyApp e ra =
  let dir := pkgBinDir e ra.name ra.pkg
   in do
     debug e "Copying application to \{dir}"
     mkDir dir
     sys "cp -r \{buildPath ra.desc}/exec/* \{dir}"

pthStr : Config e -> Bool -> String
pthStr _ False = ""
pthStr c True = """
  export IDRIS2_PACKAGE_PATH="$(\{packExec c} package-path)"
  export IDRIS2_LIBS="$(\{packExec c} libs-path)"
  export IDRIS2_DATA="$(\{packExec c} data-path)"
  """
-- When linking to a binary from pack's `bin` directory,
-- we distinguish between applications,
-- which need acceess to the Idris package path and those,
-- which don't. For the former, we create a wrapper script
-- where we first set the `IDRIS2_PACKAGE_PATH` variable
-- before invoking the binary. For both cases, we let pack
-- decide which version to use.
appLink :  HasIO io
        => Config e
        -> (exec        : Body)
        -> (app         : PkgName)
        -> (withPkgPath : Bool)
        -> EitherT PackErr io ()
appLink c exec app withPkgPath =
  let pack    := packExec c
      target  := MkF (packBinDir c) exec
      content := """
      #!/bin/sh

      APPLICATION="$(\{packExec c} app-path \{app})"
      \{pthStr c withPkgPath}

      $APPLICATION "$@"
      """
   in write target content >> sys "chmod +x \{target}"

installCmd : (withSrc : Bool) -> String
installCmd True  = "--install-with-src"
installCmd False = "--install"

||| Use the installed Idris to run an operation on
||| a library `.ipkg` file.
export
libPkg :  HasIO io
       => Env HasIdris
       -> (env  : List (String,String))
       -> (cmd  : String)
       -> (desc : Desc Safe)
       -> EitherT PackErr io ()
libPkg e env cmd desc =
  let exe := idrisWithCG e
      s   := "\{exe} \{cmd} \{desc.path.file}"
   in do
     pre <- (env ++) <$> buildEnv e
     debug e "About to run: \{s}"
     inDir (desc.path.parent) (\_ => sysWithEnv s pre)

--------------------------------------------------------------------------------
--          Installing Idris
--------------------------------------------------------------------------------

||| Builds and installs the Idris commit given in the environment.
export covering
mkIdris : HasIO io => Env DBLoaded -> EitherT PackErr io (Env HasIdris)
mkIdris e = do
  debug e "Checking Idris installation"
  when !(missing $ idrisInstallDir e) $ do
    debug e "No Idris compiler found. Installing..."
    withCoreGit e $ \dir => do
      sys "make bootstrap \{prefixVar e} \{schemeVar e}"
      sys "make install-support \{prefixVar e}"
      sys "make install-idris2 \{prefixVar e}"
      sys "make clean-libs"
      sys "rm -r build/ttc build/exec"
      cacheCoreIpkgFiles e dir

  appLink e "idris2" "idris2" True
  pure $ {db $= id} e

--------------------------------------------------------------------------------
--          Installing Libs
--------------------------------------------------------------------------------

installImpl :  HasIO io
            => Env HasIdris
            -> (dir : Path Abs)
            -> SafeLib
            -> EitherT PackErr io ()
installImpl e dir rl =
  let pre := libInstallPrefix e rl
      cmd := installCmd e.withSrc
   in do
     info e "Installing library: \{name rl}"
     libPkg e pre cmd rl.desc
     when e.withDocs $ libPkg e pre "--mkdoc" rl.desc
     when !(exists $ dir /> "lib") $
       copyDir (dir /> "lib") (pkgLibDir e rl.name rl.pkg)

export
installLib :  HasIO io => Env HasIdris -> SafeLib -> EitherT PackErr io ()
installLib e rl = case rl.status of
  Installed => pure ()
  _         => withPkgEnv e rl.name rl.pkg $ \dir =>
    let ipkgAbs := ipkg dir rl.pkg
     in case rl.pkg of
          GitHub u c ipkg _ => do
            let cache := ipkgCachePath e rl.name c ipkg
            copyFile cache ipkgAbs
            installImpl e dir rl

          Local _ _ _ => do
            installImpl e dir rl
            write (libTimestamp e rl.name rl.pkg) ""

          Core c => do
            let cache   := coreCachePath e c
            copyFile cache ipkgAbs
            case c of
              IdrisApi => sys "make src/IdrisPaths.idr"
              _        => pure ()
            installImpl e dir rl

--------------------------------------------------------------------------------
--          Installing Apps
--------------------------------------------------------------------------------

||| Install an Idris application given as a package name
export covering
installApp :  HasIO io
           => Env HasIdris
           -> SafeApp
           -> EitherT PackErr io ()
installApp e ra = case ra.status of
  Installed => pure ()
  _         => withPkgEnv e ra.name ra.pkg $ \dir =>
    let ipkgAbs := ipkg dir ra.pkg
     in case ra.pkg of
          Core _            => pure ()
          GitHub u c ipkg b => do
            let cache   := ipkgCachePath e ra.name c ipkg
            copyFile cache ipkgAbs
            libPkg e [] "--build" (notPackIsSafe ra.desc)
            copyApp e ra
            appLink e ra.exec ra.name (usePackagePath ra)
          Local _ _ b    => do
            libPkg e [] "--build" (notPackIsSafe ra.desc)
            copyApp e ra
            appLink e ra.exec ra.name (usePackagePath ra)
            write (appTimestamp e ra.name ra.pkg) ""


export covering
installAny :  HasIO io
           => Env HasIdris
           -> SafePkg
           -> EitherT PackErr io ()
installAny e (Left sli)  = installLib e sli
installAny e (Right sla) = installApp e sla

--------------------------------------------------------------------------------
--          Generating API Docs
--------------------------------------------------------------------------------

covering
docsImpl : HasIO io => Env HasIdris -> SafeLib -> EitherT PackErr io ()

docsImpl e rl = do
  let docsDir : Path Abs
      docsDir = buildPath rl.desc /> "docs"

      htmlDir : Path Abs
      htmlDir = docsDir /> "docs"

  when e.useKatla $ do
    info e "Building source docs for: \{name rl}"
    rp <- resolveApp e "katla"
    let katla := pkgExec e rp.name rp.pkg rp.exec
    fs <- map (MkF htmlDir) <$> htmlFiles htmlDir
    for_ fs $ \htmlFile =>
      let Just ds@(MkDS _ src ttm srcHtml) := sourceForDoc rl.desc htmlFile
            | Nothing => pure ()
       in sys "\{katla} html \{src} \{ttm} > \{srcHtml}" >>
          insertSources ds

  let docs := pkgDocs e rl.name rl.pkg
  when !(exists docs) (rmDir docs)
  copyDir docsDir docs

export
installDocs : HasIO io => Env HasIdris -> SafeLib -> EitherT PackErr io ()
installDocs e rl =
  withPkgEnv e rl.name rl.pkg $ \dir => docsImpl e rl

katla : Env e -> List (PkgType, PkgName)
katla e = if e.withDocs && e.useKatla then [(Bin, "katla")] else []

autoPairs : Env e -> List (PkgType, PkgName)
autoPairs e = map (Lib,) e.autoLibs ++ map (Bin,) e.autoApps

libInfo : List SafePkg -> List String
libInfo = mapMaybe $ \case Left rl => Just "\{rl.name}"
                           Right _ => Nothing

appInfo : List SafePkg -> List String
appInfo = mapMaybe $ \case Right ra => Just "\{ra.name}"
                           Left _   => Nothing

export covering
install :  HasIO io
        => Env HasIdris
        -> List (PkgType, PkgName)
        -> EitherT PackErr io ()
install e ps = do
  all <- plan e $ katla e <+> autoPairs e <+> ps
  logMany e Info "Installing libraries:" (libInfo all)
  logMany e Info "Installing apps:" (appInfo all)
  for_ all $ installAny e

  when e.withDocs $
    for_ all $ \case Left rl => installDocs e rl
                     Right _ => pure ()

export covering
installDeps :  HasIO io
            => Env HasIdris
            -> Desc Safe
            -> EitherT PackErr io ()
installDeps e = install e . map (Lib,) . dependencies

||| Creates a packaging environment with Idris2 installed.
export covering
idrisEnv : HasIO io => Config Nothing -> EitherT PackErr io (Env HasIdris)
idrisEnv c = env c >>= mkIdris

export covering
update : HasIO io => Env HasIdris -> EitherT PackErr io ()
update e =
  let dir  := packClone e
      ipkg := MkF dir "pack.ipkg"
      bin  := packBinDir e
   in finally (rmDir dir) $ do
        info e """
          Updating pack. If this fails, try switching to the latest
          package collection.
          """
        gitClone packRepo dir

        -- we're cheating here...
        d <- parseLibIpkg {s = HasIdris} ({safetyPrompt := False} e) ipkg ipkg
        installDeps e d
        inDir dir $ \_ => do
          vers <- MkCommit <$> sysRun "git rev-parse HEAD"
          let installDir    := packInstallDir e vers
              installedExec := installDir /> "pack"
          ex <- exists installedExec
          case ex of
            True  => link installedExec (packExec e)
            False => do
              libPkg e [] "--build" d
              mkDir installDir
              sys "cp -r build/exec/* \{installDir}"
              link installedExec (packExec e)

--------------------------------------------------------------------------------
--          Removing Libs
--------------------------------------------------------------------------------

removeApp : HasIO io => Env s -> PkgName -> EitherT PackErr io ()
removeApp e n = do
  info e "Removing application \{n}"
  ra <- resolveApp e n
  rmFile (pathExec e ra.exec)
  rmDir (pkgBinDir e ra.name ra.pkg)

removeLib : HasIO io => Env s -> PkgName -> EitherT PackErr io ()
removeLib e n = do
  rl <- resolveLib e n
  info e "Removing library \{n}"
  rmDir (pkgInstallDir e rl.name rl.pkg rl.desc)

||| Remove a library or executable.
export
remove : HasIO io => Env s -> List (PkgType,PkgName) -> EitherT PackErr io ()
remove e = traverse_ $ \case (Lib,n) => removeLib e n
                             (Bin,n) => removeApp e n
