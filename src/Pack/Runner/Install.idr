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

packExec : HasIO io => Env e -> EitherT PackErr io AbsFile
packExec e = do
  rp <- resolve e "pack"
  Just p <- pure (packageExec e rp) | Nothing => throwE $ NoApp "pack"
  pure p

packInstalled : HasIO io => Env e -> EitherT PackErr io Bool
packInstalled e = packExec e >>= exists . path

||| Use the installed Idris to run an operation on an `.ipkg` file.
export
idrisPkg :  HasIO io
         => Env HasIdris
         -> (env : List (String,String))
         -> (cmd : String)
         -> AbsFile
         -> EitherT PackErr io ()
idrisPkg e env cmd ipkg =
  let exe := idrisWithCG e
      pre := env ++ buildEnv e
      s   := "\{exe} \{cmd} \{ipkg}"
   in do
     debug e "About to run: \{s}"
     sysWithEnv s pre

buildDir : PkgDesc -> FilePath
buildDir d = maybe "build" fromString d.builddir

copyApp : HasIO io => Env HasIdris -> ResolvedPackage -> EitherT PackErr io ()
copyApp e rp =
  let dir = packageBinDir e rp
   in do
        debug e "Copying application to \{dir}" >>
        mkDir dir
        sys "cp -r \{buildDir $ desc rp}/exec/* \{dir}"

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
        => (exec     : AbsFile)
        -> (target   : AbsFile)
        -> (packPath : Maybe AbsFile)
        -> EitherT PackErr io ()
appLink (MkAF p b) target Nothing  = link (p /> b) target
appLink exec target (Just p)       =
  let content = """
      #!/bin/sh

      export IDRIS2_PACKAGE_PATH="$(\{p} package-path)"
      export IDRIS2_LIBS="$(\{p} libs-path)"
      export IDRIS2_DATA="$(\{p} data-path)"
      "\{exec}" "$@"
      """
   in write target content >> sys "chmod +x \{target}"

||| Builds and installs the Idris commit given in the environment.
export
mkIdris : HasIO io => Env DBLoaded -> EitherT PackErr io (Env HasIdris)
mkIdris e = do
  debug e "Checking Idris installation"
  when !(missing $ idrisInstallDir e) $ do
    debug e "No Idris compiler found. Installing..."
    if e.bootstrap
       then -- build with bootstrapping
         withGit (tmpDir e) e.db.idrisURL e.db.idrisCommit $ \dir => do
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

       else -- build with existing Idris2 compiler
         withGit (tmpDir e) e.db.idrisURL e.db.idrisCommit $ \dir => do
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

installCmd : (withSrc : Bool) -> String
installCmd True  = "--install-with-src"
installCmd False = "--install"

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

promptDesc :  HasIO io
           => ResolvedPackage
           -> Config s
           -> PkgDesc
           -> io Bool
promptDesc = prompt . name


||| Install the given library with all its dependencies.
export covering
installLib :  HasIO io
           => Env HasIdris
           -> PkgName
           -> EitherT PackErr io ()
installLib e n = do
  debug e "Installing library \{n}..."
  rp <- resolve e n
  traverse_ (installLib e) (dependencies rp)
  case rp of
    RGitHub pn url commit ipkg _ d => do
      False <- packageUpToDate e rp | True => pure ()
      when !(promptDesc rp e d) $
        withGit (tmpDir e) url commit $ \dir => do
          let ipkgAbs := toAbsFile dir ipkg
              pf      := patchFile e n ipkg
          when !(exists $ path pf) (patch ipkgAbs pf)
          idrisPkg e (packageInstallPrefix e rp)
                     (installCmd e.withSrc)
                     ipkgAbs
    p@(RLocal _ ipkg _ d) => do
      let ts := localTimestamp e p
      False <- localUpToDate e rp | True => pure ()
      when !(promptDesc rp e d) $ do
        idrisPkg e (packageInstallPrefix e rp)
                   (installCmd e.withSrc)
                   ipkg
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

covering
runIdrisOn :  HasIO io
           => (cmd : Maybe String)
           -> AbsFile
           -> Env HasIdris
           -> EitherT PackErr io ()
runIdrisOn cmd p e = do
  (_,d) <- parseIpkgFile p id
  traverse_ (installLib e) (dependencies d)
  case cmd of
    Just c  => idrisPkg e [] c p
    Nothing => pure ()

findIpkg :  HasIO io
         => WithIpkg
         -> Maybe AbsFile
         -> EitherT PackErr io (Maybe AbsFile)
findIpkg (Search dir) fi =
  let searchDir := maybe dir parent fi
   in findInParentDirs isIpkgBody searchDir
findIpkg None         _  = pure Nothing
findIpkg (Use x)      _  = pure (Just x)

covering
replOpts :  HasIO io
         => Env HasIdris
         -> (file : Maybe AbsFile)
         -> EitherT PackErr io (String, Maybe AbsFile)
replOpts e mf = do
  Just p <- findIpkg e.withIpkg mf | Nothing => pure ("",Nothing)
  info e "Found `.ipkg` file at \{p}"
  (_,desc) <- parseIpkgFile p id
  traverse_ (installLib e) (dependencies desc)
  let srcDir = maybe "" (\s => "--source-dir \"\{s}\"") desc.sourcedir
      pkgs = unwords $ map (("-p " ++) . pkgname) desc.depends
  pure ("\{srcDir} \{pkgs}", Just p)

||| Use the installed Idris to start a REPL session with the
||| given argument string.
export covering
idrisRepl :  HasIO io
          => (file : Maybe AbsFile)
          -> Env HasIdris
          -> EitherT PackErr io ()
idrisRepl file e = do
  let args := maybe "" interpolate $ file
      pth  := packagePath e
      exe  := idrisWithCG e

  (opts, mp) <- replOpts e file

  cmd <- case e.rlwrap of
    True  => pure "rlwrap \{exe} \{opts} \{args}"
    False => pure "\{exe} \{opts} \{args}"

  case mp of
    Just af => inDir af.parent $ \_ => sysWithEnv cmd [pth]
    Nothing => sysWithEnv cmd [pth]

||| Build a local library given as an `.ipkg` file.
export covering
build : HasIO io => AbsFile -> Env HasIdris -> EitherT PackErr io ()
build = runIdrisOn (Just "--build")

||| Install dependencies of a local `.ipkg` file
export covering
buildDeps : HasIO io => AbsFile -> Env HasIdris -> EitherT PackErr io ()
buildDeps = runIdrisOn Nothing

||| Typecheck a local library given as an `.ipkg` file.
export covering
typecheck : HasIO io => AbsFile -> Env HasIdris -> EitherT PackErr io ()
typecheck = runIdrisOn (Just "--typecheck")

||| Install an Idris application given as a package name
export covering
installApp :  HasIO io
           => Env HasIdris
           -> PkgName
           -> EitherT PackErr io ()
installApp e n = do
  debug e "Installing application \{n}..."
  rp       <- resolve e n
  Just exe <- pure (packageExec e rp) | Nothing => throwE (NoApp n)
  traverse_ (installLib e) (dependencies rp)
  case rp of
    RGitHub pn url commit ipkg pp d => do
      False <- executableExists e exe | True => pure ()
      when !(promptDesc rp e d) $
        withGit (tmpDir e) url commit $ \dir => do
          let ipkgAbs := toAbsFile dir ipkg
              pf      := patchFile e n ipkg
          when !(exists $ path pf) (patch ipkgAbs pf)
          idrisPkg e [] "--build" ipkgAbs
          copyApp e rp

    RLocal _ ipkg pp d => when !(promptDesc rp e d) $ do
      removeExec e rp exe.file
      idrisPkg e [] "--build" ipkg
      copyApp e rp
    _ => throwE (NoApp n)

  case usePackagePath rp of
    True  => packExec e >>= appLink exe (collectionAppExec e exe.file) . Just
    False => appLink exe (collectionAppExec e exe.file) Nothing

execPath : AbsFile -> PkgDesc -> Maybe (Path Abs)
execPath p d = do
  exe <- d.executable
  bod <- body exe
  pure (toAbsPath p.parent (buildDir d) /> "exec" /> bod)

||| Install and run an executable given as a package name.
export covering
runIpkg :  HasIO io
        => AbsFile
        -> (args : List String)
        -> Env HasIdris
        -> EitherT PackErr io ()
runIpkg p args e = do
  (_,d)    <- parseIpkgFile p id
  Just exe <- pure (execPath p d) | Nothing => throwE (NoAppIpkg p)
  build p e
  sys "\{exe} \{unwords args}"

||| Install and run an executable given as a package name.
export covering
execApp :  HasIO io
        => PkgName
        -> (args : List String)
        -> Env HasIdris
        -> EitherT PackErr io ()
execApp p args e = do
  rp@(RGitHub {}) <- resolve e p
    | RLocal _ ipkg _ _ => runIpkg ipkg args e
    | Core {}           => throwE (NoApp p)
  Just exe <- pure (packageExec e rp) | Nothing => throwE (NoApp p)
  installApp e p
  sys "\{exe} \{unwords args}"

||| Creates a packaging environment with Idris2 installed.
export covering
idrisEnv : HasIO io => Config Nothing -> EitherT PackErr io (Env HasIdris)
idrisEnv c = do
  e <- env c >>= mkIdris
  traverse_ (installLib e) e.autoLibs
  traverse_ (installApp e) e.autoApps
  pure e
