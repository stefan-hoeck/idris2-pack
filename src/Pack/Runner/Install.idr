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

-- return the path to the `pack` executable corresponding
-- to the current collection
packExec : HasIO io => Env e -> EitherT PackErr io (File Abs)
packExec e = do
  rp     <- resolve e "pack"
  Just p <- pure (packageExec e rp) | Nothing => throwE $ NoApp "pack"
  pure p

-- True if the `pack` executable exists.
packInstalled : HasIO io => Env e -> EitherT PackErr io Bool
packInstalled e = packExec e >>= fileExists

copyApp :  HasIO io
        => Env HasIdris
        -> (ipkg : File Abs)
        -> (desc : PkgDesc)
        -> ResolvedPackage
        -> EitherT PackErr io ()
copyApp e ipkg d rp =
  let dir = packageBinDir e rp
   in do
     debug e "Copying application to \{dir}"
     mkDir dir
     sys "cp -r \{buildPath ipkg d}/exec/* \{dir}"

export
links : HasIO io => Env HasIdris -> EitherT PackErr io ()
links e = do
  debug e "Creating sym links"
  link (collectionBinDir e) (packBinDir e)

-- When linking to a binary from a package collection's
-- `bin` directory, we distinguish between applications,
-- which need acceess to the Idris package path and those,
-- which don't. For the former, we create a wrapper script
-- where we first set the `IDRIS2_PACKAGE_PATH` variable
-- before invoking the binary, for the latter we create just
-- a symlink.
appLink :  HasIO io
        => (exec     : File Abs)
        -> (target   : File Abs)
        -> (packPath : Maybe $ File Abs)
        -> EitherT PackErr io ()
appLink (MkF p b) target Nothing  = link (p /> b) target
appLink exec target (Just p)       =
  let content = """
      #!/bin/sh

      export IDRIS2_PACKAGE_PATH="$(\{p} package-path)"
      export IDRIS2_LIBS="$(\{p} libs-path)"
      export IDRIS2_DATA="$(\{p} data-path)"
      "\{exec}" "$@"
      """
   in write target content >> sys "chmod +x \{target}"

installCmd : (withSrc : Bool) -> String
installCmd True  = "--install-with-src"
installCmd False = "--install"

||| Use the installed Idris to run an operation on an `.ipkg` file.
export
idrisPkg :  HasIO io
         => Env HasIdris
         -> (env  : List (String,String))
         -> (cmd  : String)
         -> (ipkg : File Abs)
         -> (desc : PkgDesc)
         -> EitherT PackErr io ()
idrisPkg e env cmd ipkg d =
  let exe := idrisWithCG e
      pre := env ++ buildEnv e
      s   := "\{exe} \{cmd} \{ipkg}"
   in debug e "About to run: \{s}" >>
      sysWithEnv s pre

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
      case e.bootstrap of
        True => do
          sys "make bootstrap \{prefixVar e} \{schemeVar e}"
          sys "make install-support \{prefixVar e}"
          sys "make install-idris2 \{prefixVar e}"
        False => do
          sys "make support"
          sys "make install-support \{prefixVar e}"
          sys "make idris2-exec IDRIS2_INC_CGS=\"\" \{idrisBootVar e}"
          sys "make install-idris2 \{idrisBootVar e} \{prefixVar e}"
      sys "make clean-libs"
      sys "rm -r build/ttc build/exec"
      cacheCoreIpkgFiles e dir

  exepath <- packExec e
  appLink (idrisExec e) (collectionIdrisExec e) (Just exepath)
  pure $ {db $= id} e

--------------------------------------------------------------------------------
--          Installing Libs
--------------------------------------------------------------------------------

installImpl :  HasIO io
            => Env HasIdris
            -> ResolvedPackage
            -> (ipkg : File Abs)
            -> (desc : PkgDesc)
            -> EitherT PackErr io ()
installImpl e rp ipkg d =
  let pre := packageInstallPrefix e rp
      cmd := installCmd e.withSrc
   in do
     idrisPkg e pre cmd ipkg d
     when e.withDocs $ idrisPkg e pre "--mkdoc" ipkg d

||| Install the given library with all its dependencies.
export
installLib :  HasIO io
           => Env HasIdris
           -> ResolvedPackage
           -> EitherT PackErr io ()
installLib e rp = do
  info e "Installing library: \{name rp}"
  case rp of
    RGitHub pn url commit ipkg _ d =>
      withGit (tmpDir e) pn url commit $ \dir => do
        let ipkgAbs := toAbsFile dir ipkg
            cache   := ipkgPath e pn commit ipkg
        copyFile cache ipkgAbs
        installImpl e rp ipkgAbs d

    p@(RLocal pn ipkg _ d) => do
      installImpl e rp ipkg d
      write (localTimestamp e p) ""
    Core c d               =>
      withCoreGit e $ \dir => do
        let ipkgAbs := toAbsFile dir (coreIpkgPath c)
            cache   := coreCachePath e c
        copyFile cache ipkgAbs
        case c of
          IdrisApi => sys "make src/IdrisPaths.idr"
          _        => pure ()
        installImpl e rp ipkgAbs d

--------------------------------------------------------------------------------
--          Installing Apps
--------------------------------------------------------------------------------

removeExec :  HasIO io
           => Env s
           -> ResolvedPackage
           -> Body
           -> EitherT PackErr io ()
removeExec e rp n = do
  debug e "Removing application \{n}"
  rmFile (collectionAppExec e n)
  rmDir (packageBinDir e rp)

||| Install an Idris application given as a package name
export covering
installApp :  HasIO io
           => Env HasIdris
           -> ResolvedPackage
           -> EitherT PackErr io ()
installApp e rp = do
  info e "Installing application: \{name rp}"
  Just exe <- pure (packageExec e rp) | Nothing => throwE (NoApp $ name rp)
  case rp of
    RGitHub pn url commit ipkg pp d =>
      withGit (tmpDir e) pn url commit $ \dir => do
        let ipkgAbs := toAbsFile dir ipkg
            cache   := ipkgPath e pn commit ipkg
        copyFile cache ipkgAbs
        idrisPkg e [] "--build" ipkgAbs d
        copyApp e ipkgAbs d rp

    RLocal pn ipkg pp d => do
      removeExec e rp exe.file
      idrisPkg e [] "--build" ipkg d
      copyApp e ipkg d rp

    _ => throwE (NoApp $ name rp)

  case usePackagePath rp of
    True  => packExec e >>= appLink exe (collectionAppExec e exe.file) . Just
    False => appLink exe (collectionAppExec e exe.file) Nothing

--------------------------------------------------------------------------------
--          Generating API Docs
--------------------------------------------------------------------------------

covering
docsImpl :  HasIO io
         => Env HasIdris
         -> ResolvedPackage
         -> (ipkg : File Abs)
         -> (desc : PkgDesc)
         -> EitherT PackErr io ()

docsImpl e rp ipkg d = do
  let docsDir : Path Abs
      docsDir = buildPath ipkg d /> "docs"

      htmlDir : Path Abs
      htmlDir = docsDir /> "docs"

  when e.useKatla $ do
    info e "Building source docs for: \{name rp}"
    rp <- resolve e "katla"
    let katla := MkF (packageBinDir e rp) "katla"
    fs <- map (MkF htmlDir) <$> htmlFiles htmlDir
    for_ fs $ \htmlFile =>
      let Just ds@(MkDS _ src ttm srcHtml) := sourceForDoc ipkg d htmlFile
            | Nothing => pure ()
       in sys "\{katla} html \{src} \{ttm} > \{srcHtml}" >>
          insertSources ds

  when !(exists $ packageDocs e rp) (rmDir $ packageDocs e rp)
  copyDir docsDir (packageDocs e rp)

export
installDocs :  HasIO io
            => Env HasIdris
            -> ResolvedPackage
            -> EitherT PackErr io ()
installDocs e rp@(RGitHub pn url commit ipkg _ d) =
  inDir (gitDir (tmpDir e) pn commit) $ \dir =>
    docsImpl e rp (toAbsFile dir ipkg) d

installDocs e rp@(RLocal pn ipkg _ d) = docsImpl e rp ipkg d

installDocs e rp@(Core c d) =
  inDir (coreGitDir e) $ \dir =>
    docsImpl e rp (toAbsFile dir $ coreIpkgPath c) d

katla : Env e -> List (PkgType, PkgName)
katla e = if e.withDocs && e.useKatla then [(Bin, "katla")] else []

autoPairs : Env e -> List (PkgType, PkgName)
autoPairs e = map (Lib,) e.autoLibs ++ map (Bin,) e.autoApps

libInfo : List (PkgType, ResolvedPackage) -> String
libInfo = unlines . map (interpolate . name . snd) . filter ((Lib ==) . fst)

appInfo : List (PkgType, ResolvedPackage) -> String
appInfo = unlines . map (interpolate . name . snd) . filter ((Bin ==) . fst)

export covering
install :  HasIO io
        => Env HasIdris
        -> List (PkgType, PkgName)
        -> EitherT PackErr io ()
install e ps = do
  rs <- plan e $ katla e <+> autoPairs e <+> ps
  info e $ "Installing the following libraries:\n" ++ libInfo rs
  info e $ "Installing the following apps:\n" ++ appInfo rs
  for_ rs $ \case (Lib,rp) => installLib e rp
                  (Bin,rp) => installApp e rp

  when e.withDocs $
    for_ rs $ \case (Lib,rp) => installDocs e rp
                    (Bin,rp) => pure ()

export covering
installDeps :  HasIO io
            => Env HasIdris
            -> PkgDesc
            -> EitherT PackErr io ()
installDeps e = install e . map (Lib,) . dependencies

||| Creates a packaging environment with Idris2 installed.
export covering
idrisEnv : HasIO io => Config Nothing -> EitherT PackErr io (Env HasIdris)
idrisEnv c = env c >>= mkIdris

--------------------------------------------------------------------------------
--          Removing Libs
--------------------------------------------------------------------------------

||| Remove a library or executable.
export covering
remove : HasIO io => Env s -> PkgName -> EitherT PackErr io ()
remove env n = do
  debug env "Removing library or application \{n}..."
  rp <- resolve env n
  rmDir (packagePrefixDir env rp)
  case executable rp of
    Just exe => removeExec env rp exe
    Nothing  => pure ()
