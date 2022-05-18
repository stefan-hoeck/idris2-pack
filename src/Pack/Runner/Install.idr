module Pack.Runner.Install

import Core.FC
import Data.List1
import Data.Maybe
import Data.String
import Idris.Package.Types
import Pack.Config.Env
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types
import Pack.Runner.Database

%default total

||| Use the installed Idris to run an operation on an `.ipkg` file.
export
idrisPkg :  HasIO io
         => Env HasIdris
         -> (env : String)
         -> (cmd : String)
         -> Path
         -> EitherT PackErr io ()
idrisPkg e env cmd ipkg =
  let s = "\{env} \{buildEnv e} \{show $ idrisExec e} \{cmd} \{show ipkg}"
   in debug e "About to run: \{s}" >> sys s

copyApp : HasIO io => Env HasIdris -> ResolvedPackage -> EitherT PackErr io ()
copyApp e rp =
  let dir = packageBinDir e rp
   in do
        debug e "Copying application to \{show dir}" >>
        mkDir dir
        sys "cp -r build/exec/* \{show dir}"

export
links : HasIO io => Env HasIdris -> EitherT PackErr io ()
links e = do
  rmDir (packBinDir e)
  debug e "Creating sym links"
  link (collectionBinDir e) (packBinDir e)

||| Builds and installs the Idris commit given in the environment.
export
mkIdris : HasIO io => Env DBLoaded -> EitherT PackErr io (Env HasIdris)
mkIdris e = do
  debug e "Checking Idris installation"
  False <- exists (idrisInstallDir e) | True => pure $ {db $= id} e

  debug e "No Idris compiler found. Installing..."
  if e.bootstrap
     then -- build with bootstrapping
       withGit (tmpDir e) idrisRepo e.db.idrisCommit $ do
         sys "make bootstrap \{prefixVar e} \{schemeVar e}"
         sys "make install \{prefixVar e}"
         sys "make clean"
         sys "make all \{idrisBootVar e} \{prefixVar e}"
         sys "make install \{idrisBootVar e} \{prefixVar e}"
         sys "make install-with-src-libs \{idrisBootVar e} \{prefixVar e}"
         sys "make install-with-src-api \{idrisBootVar e} \{prefixVar e}"

     else -- build with existing Idris2 compiler
       withGit (tmpDir e) idrisRepo e.db.idrisCommit $ do
         sys "make all \{prefixVar e}"
         sys "make install \{prefixVar e}"
         sys "make install-with-src-libs \{prefixVar e}"
         sys "make clean"
         sys "make install-with-src-api \{idrisBootVar e} \{prefixVar e}"

  link (idrisExec e) (collectionIdrisExec e)
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
           -> PkgRep
           -> EitherT PackErr io ()
installLib e n = do
  debug e "Installing library \{n}..."
  rp <- resolve e n
  traverse_ (installLib e) (dependencies rp)
  case rp of
    RGitHub pn url commit ipkg d => do
      False <- packageExists e rp | True => pure ()
      when !(promptDesc rp e d) $
        withGit (tmpDir e) url commit $ do
          let pf = patchFile e pn ipkg
          when !(exists pf) (patch ipkg pf)
          idrisPkg e (packageInstallPrefix e rp) (installCmd e.withSrc) ipkg
    RIpkg ipkg d =>
      when !(promptDesc rp e d) $
        idrisPkg e (packageInstallPrefix e rp) (installCmd e.withSrc) ipkg
    RLocal _ dir ipkg d =>
      when !(promptDesc rp e d) $
        inDir dir $ idrisPkg e (packageInstallPrefix e rp) (installCmd e.withSrc) ipkg
    _             => do
      False <- packageExists e rp | True => pure ()
      throwE (MissingCorePackage (name rp) e.db.idrisVersion e.db.idrisCommit)

removeExec :  HasIO io
           => Env s
           -> ResolvedPackage
           -> String
           -> EitherT PackErr io ()
removeExec e rp n = do
  debug e "Removing application \{n}"
  rmFile (collectionAppExec e n)
  rmFile (packageExec e rp n)
  rmDir  (packageAppDir e rp n)

||| Remove a library or executable.
export covering
remove : HasIO io => Env s -> PkgRep -> EitherT PackErr io ()
remove env n = do
  debug env "Removing library or application \{n}..."
  rp <- resolve env n
  rmDir (packageInstallDir env rp)
  whenJust (executable rp) (removeExec env rp)

covering
runIdrisOn :  HasIO io => (cmd : String)
           -> Path
           -> Env HasIdris
           -> EitherT PackErr io ()
runIdrisOn cmd p e = do
  RIpkg ipkg d <- resolve e (Ipkg p) | _ => throwE BuildMany
  traverse_ (installLib e) (dependencies $ RIpkg ipkg d)
  idrisPkg e "" cmd ipkg

||| Use the installed Idris to start a REPL session with the
||| given argument string.
export covering
idrisRepl :  HasIO io
          => Env HasIdris
          -> (args : String)
          -> EitherT PackErr io ()
idrisRepl e args =
  let pth = packagePath e
   in do
        opts <- replOpts
        case e.rlwrap of
          True  => sys "\{pth} rlwrap \{show $ idrisExec e} \{opts} \{args}"
          False => sys "\{pth} \{show $ idrisExec e} \{opts} \{args}"

  where covering replOpts : EitherT PackErr io String
        replOpts = case e.withIpkg of
          Nothing => pure ""
          Just p  => do
            RIpkg ipkg d <- resolve e (Ipkg p) | _ => throwE BuildMany
            traverse_ (installLib e) (dependencies $ RIpkg ipkg d)
            let srcDir = maybe "" (\s => "--source-dir \"\{s}\"") d.sourcedir
                pkgs = unwords $ map (("-p " ++) . pkgname) d.depends
            pure "\{srcDir} \{pkgs}"

||| Build a local library given as an `.ipkg` file.
export covering
build : HasIO io => Path -> Env HasIdris -> EitherT PackErr io ()
build = runIdrisOn "--build"

||| Typecheck a local library given as an `.ipkg` file.
export covering
typecheck : HasIO io => Path -> Env HasIdris -> EitherT PackErr io ()
typecheck = runIdrisOn "--typecheck"

||| Load an optional file into a REPL session
export covering
repl :  HasIO io
     => Maybe Path
     -> Env HasIdris
     -> EitherT PackErr io ()
repl Nothing e  = idrisRepl e ""
repl (Just p) e = idrisRepl e (show p)

||| Install an Idris application given as a package name
||| or a path to a local `.ipkg` file.
export covering
installApp :  HasIO io
           => Env HasIdris
           -> PkgRep
           -> EitherT PackErr io ()
installApp e n = do
  debug e "Installing application \{n}..."
  rp       <- resolve e n
  Just exe <- pure (executable rp) | Nothing => throwE (NoApp n)
  traverse_ (installLib e) (dependencies rp)
  case rp of
    RGitHub pn url commit ipkg d => do
      False <- executableExists e rp exe | True => pure ()
      when !(promptDesc rp e d) $
        withGit (tmpDir e) url commit $ do
          let pf = patchFile e pn ipkg
          when !(exists pf) (patch ipkg pf)
          idrisPkg e "" "--build" ipkg
          copyApp e rp
          link (packageExec e rp exe) (collectionAppExec e exe)

    RIpkg ipkg d => when !(promptDesc rp e d) $ do
      removeExec e rp exe
      idrisPkg e "" "--build" ipkg
      copyApp e rp
      link (packageExec e rp exe) (collectionAppExec e exe)

    RLocal _ dir ipkg d => when !(promptDesc rp e d) $ do
      removeExec e rp exe
      inDir dir $ do
        idrisPkg e "" "--build" ipkg
        copyApp e rp
        link (packageExec e rp exe) (collectionAppExec e exe)
    _ => throwE (NoApp n)

||| Build and run an executable given either
||| as an `.ipkg` file or an application from the
||| package collection.
export covering
execApp :  HasIO io
        => PkgRep
        -> (args : List String)
        -> Env HasIdris
        -> EitherT PackErr io ()
execApp p args e = do
  rp       <- resolve e p
  Just exe <- pure (executable rp) | Nothing => throwE (NoApp p)
  case rp of
    RIpkg ipkg d => do
      traverse_ (installLib e) (dependencies rp)
      idrisPkg e "" "--build" ipkg
      sys "build/exec/\{exe} \{unwords args}"
    _            => do
      installApp e p
      sys "\{show $ packageExec e rp exe} \{unwords args}"

||| Creates a packaging environment with Idris2 installed.
export covering
idrisEnv : HasIO io => Config Nothing -> EitherT PackErr io (Env HasIdris)
idrisEnv c = do
  e <- env c >>= mkIdris
  traverse_ (installLib e) (map Pkg e.autoLibs)
  traverse_ (installApp e) (map Pkg e.autoApps)
  pure e
