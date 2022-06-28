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
        debug e "Copying application to \{dir}" >>
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
       putStrLn "Package \{n} uses custom build hooks. Continue (yes/*no)?"
       "yes" <- trim <$> getLine | _ => putStrLn "Aborting..." $> False
       pure True
     else pure True

installCmd : (withSrc : Bool) -> String
installCmd True  = "--install-with-src"
installCmd False = "--install"

--------------------------------------------------------------------------------
--          Installing Libs
--------------------------------------------------------------------------------

||| Use the installed Idris to run an operation on an `.ipkg` file.
export covering
idrisPkg :  HasIO io
         => Env HasIdris
         -> (env  : List (String,String))
         -> (cmd  : String)
         -> (ipkg : File Abs)
         -> (desc : PkgDesc)
         -> EitherT PackErr io ()

||| Builds and installs the Idris commit given in the environment.
export
mkIdris : HasIO io => Env DBLoaded -> EitherT PackErr io (Env HasIdris)

covering
installImpl :  HasIO io
            => Env HasIdris
            -> ResolvedPackage
            -> (ipkg : File Abs)
            -> (desc : PkgDesc)
            -> EitherT PackErr io ()

||| Install the given library with all its dependencies.
export covering
installLib :  HasIO io
           => Env HasIdris
           -> PkgName
           -> EitherT PackErr io ()

||| Install an Idris application given as a package name
export covering
installApp :  HasIO io
           => Env HasIdris
           -> PkgName
           -> EitherT PackErr io ()

--------------------------------------------------------------------------------
--          Installation Implementations
--------------------------------------------------------------------------------

mkIdris e = do
  debug e "Checking Idris installation"
  when !(missing $ idrisInstallDir e) $ do
    debug e "No Idris compiler found. Installing..."
    withGit (tmpDir e) e.db.idrisURL e.db.idrisCommit $ \dir =>
      if e.bootstrap
        then do -- build with bootstrapping
          sys "make bootstrap \{prefixVar e} \{schemeVar e}"
          sys "make install \{prefixVar e}"
          sys "make clean-libs"
          sys "rm -r build/ttc build/exec"
          sys "make idris2-exec \{idrisBootVar e} \{prefixVar e} IDRIS2_INC_CGS=\"\""
          sys "make libs \{idrisBootVar e} \{prefixVar e}"
          sys "make install \{idrisBootVar e} \{prefixVar e}"
          sys "make install-with-src-libs \{idrisBootVar e} \{prefixVar e}"
          sys "rm -r build/ttc build/exec"
          sys "make install-with-src-api \{idrisBootVar e} \{prefixVar e}"
          cacheCoreIpkgFiles e dir

        else do -- build with existing Idris2 compiler
          sys "make support \{prefixVar e}"
          sys "make idris2-exec \{prefixVar e} IDRIS2_INC_CGS=\"\""
          sys "make libs \{prefixVar e}"
          sys "make install \{prefixVar e}"
          sys "make install-with-src-libs \{prefixVar e}"
          sys "rm -r build/ttc build/exec"
          sys "make install-with-src-api \{idrisBootVar e} \{prefixVar e}"
          cacheCoreIpkgFiles e dir

  exepath <- packExec e
  appLink (idrisExec e) (collectionIdrisExec e) (Just exepath)
  pure $ {db $= id} e

idrisPkg e env cmd ipkg d =
  let exe := idrisWithCG e
      pre := env ++ buildEnv e
      s   := "\{exe} \{cmd} \{ipkg}"
   in do
     traverse_ (installLib e) (dependencies d)
     debug e "About to run: \{s}"
     sysWithEnv s pre

installImpl e rp ipkg d = do
  idrisPkg e (packageInstallPrefix e rp)
             (installCmd e.withSrc)
             ipkg
             d
  when e.withDocs $ do
    let docsDir : Path Abs
        docsDir = buildPath ipkg d /> "docs"

        htmlDir : Path Abs
        htmlDir = docsDir /> "docs"

    idrisPkg e (packageInstallPrefix e rp) "--mkdoc" ipkg d
    when e.useKatla $ do
      installApp e "katla"
      fs <- map (MkF htmlDir) <$> htmlFiles htmlDir
      for_ fs $ \htmlFile =>
        let Just ds@(MkDS _ src ttm srcHtml) := sourceForDoc ipkg d htmlFile
              | Nothing => pure ()
         in sys "katla html \{src} \{ttm} > \{srcHtml}" >>
            insertSources ds

    copyDir docsDir (packageDocs e rp)


installLib e n = do
  debug e "Installing library \{n}..."
  rp <- resolve e n
  case rp of
    RGitHub pn url commit ipkg _ d => do
      False <- packageUpToDate e rp | True => pure ()
      when !(prompt pn e d) $
        withGit (tmpDir e) url commit $ \dir => do
          let ipkgAbs := toAbsFile dir ipkg
              pf      := patchFile e n ipkg
          when !(fileExists pf) (patch ipkgAbs pf)
          installImpl e rp ipkgAbs d
    p@(RLocal pn ipkg _ d) => do
      let ts := localTimestamp e p
      False <- localUpToDate e rp | True => pure ()
      when !(prompt pn e d) $ do
        installImpl e rp ipkg d
        write ts ""
    _             => do
      False <- packageUpToDate e rp | True => pure ()
      throwE (MissingCorePackage (name rp) e.db.idrisVersion e.db.idrisCommit)

removeExec :  HasIO io
           => Env s
           -> ResolvedPackage
           -> Body
           -> EitherT PackErr io ()
removeExec e rp n = do
  debug e "Removing application \{n}"
  rmFile (collectionAppExec e n)
  rmDir (packageBinDir e rp)

installApp e n = do
  debug e "Installing application \{n}..."
  rp       <- resolve e n
  Just exe <- pure (packageExec e rp) | Nothing => throwE (NoApp n)
  case rp of
    RGitHub pn url commit ipkg pp d => do
      False <- executableExists e exe | True => pure ()
      when !(prompt pn e d) $
        withGit (tmpDir e) url commit $ \dir => do
          let ipkgAbs := toAbsFile dir ipkg
              pf      := patchFile e n ipkg
          when !(fileExists pf) (patch ipkgAbs pf)
          idrisPkg e [] "--build" ipkgAbs d
          copyApp e ipkgAbs d rp

    RLocal pn ipkg pp d => when !(prompt pn e d) $ do
      removeExec e rp exe.file
      idrisPkg e [] "--build" ipkg d
      copyApp e ipkg d rp
    _ => throwE (NoApp n)

  case usePackagePath rp of
    True  => packExec e >>= appLink exe (collectionAppExec e exe.file) . Just
    False => appLink exe (collectionAppExec e exe.file) Nothing

||| Creates a packaging environment with Idris2 installed.
export covering
idrisEnv : HasIO io => Config Nothing -> EitherT PackErr io (Env HasIdris)
idrisEnv c = do
  e <- env c >>= mkIdris
  traverse_ (installLib e) e.autoLibs
  traverse_ (installApp e) e.autoApps
  pure e

--------------------------------------------------------------------------------
--          Installing Libs
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
