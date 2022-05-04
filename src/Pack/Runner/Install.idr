module Pack.Runner.Install

import Data.List1
import Data.String
import Idris.Package.Types
import Pack.CmdLn.Env
import Pack.CmdLn.Types
import Pack.Core
import Pack.Database.Types
import Pack.Runner.Database

%default total

||| Use the installed Idris to run an operation on an `.ipkg` file.
export
idrisPkg :  HasIO io
         => Env HasIdris
         -> (cmd : String)
         -> Path
         -> EitherT PackErr io ()
idrisPkg e cmd ipkg = sys "\{show $ idrisExec e} \{cmd} \{show ipkg}"

copyApp : HasIO io => Env HasIdris -> EitherT PackErr io ()
copyApp e = sys "cp -r build/exec/* \{show $ idrisBinDir e}"

||| Builds and installs the Idris commit given in the environment.
export
mkIdris : HasIO io => Env DBLoaded -> EitherT PackErr io (Env HasIdris)
mkIdris e@(MkConfig pd v s c db) = do
  False <- exists (idrisInstallDir e) | True => pure $ MkConfig pd v s c db

  withGit (tmpDir e) idrisRepo db.idrisCommit $ do
    sys "make bootstrap \{prefixVar e} \{schemeVar e}"
    sys "make install \{prefixVar e}"
    sys "make clean"
    sys "make all \{idrisBootVar e} \{prefixVar e}"
    sys "make install \{idrisBootVar e} \{prefixVar e}"
    sys "make install-with-src-libs \{idrisBootVar e} \{prefixVar e}"
    sys "make install-with-src-api \{idrisBootVar e} \{prefixVar e}"
    pure $ MkConfig pd v s c db

||| Creates a packaging environment with Idris2 installed.
export
idrisEnv : HasIO io => Config Nothing -> EitherT PackErr io (Env HasIdris)
idrisEnv c = env c >>= mkIdris

installCmd : (withSrc : Bool) -> String
installCmd True  = "--install-with-src"
installCmd False = "--install"

||| Install the given library with all its dependencies.
export covering
installLib :  HasIO io
           => (withSrc : Bool)
           -> Env HasIdris
           -> PkgRep
           -> EitherT PackErr io ()
installLib ws e n = do
  rp <- resolve e n
  traverse_ (installLib ws e) (dependencies rp)
  case rp of
    RGitHub pn url commit ipkg d => do
      False <- packageExists e rp | True => pure ()
      withGit (tmpDir e) url commit $ do
        let pf = patchFile e pn ipkg
        when !(exists pf) (patch ipkg pf)
        idrisPkg e (installCmd ws) ipkg
    RIpkg ipkg d => idrisPkg e (installCmd ws) ipkg
    RLocal _ dir ipkg d => inDir dir $ idrisPkg e (installCmd ws) ipkg
    _             => do
      False <- packageExists e rp | True => pure ()
      throwE (MissingCorePackage (name rp) e.db.idrisVersion e.db.idrisCommit)

removeExec : HasIO io => Env s -> String -> EitherT PackErr io ()
removeExec e n = do
  rmFile (packageExec e n)
  rmDir  (packageAppDir e n)

||| Remove a library or executable.
export covering
remove : HasIO io => Env s -> PkgRep -> EitherT PackErr io ()
remove env n = do
  rp <- resolve env n
  rmDir (packageInstallDir env rp)
  whenJust (executable rp) (removeExec env)

covering
runIdrisOn :  HasIO io => (cmd : String)
           -> Path
           -> Env HasIdris
           -> EitherT PackErr io ()
runIdrisOn cmd p e = do
  RIpkg ipkg d <- resolve e (Ipkg p) | _ => throwE BuildMany
  traverse_ (installLib True e) (dependencies $ RIpkg ipkg d)
  idrisPkg e cmd ipkg

||| Build a local library given as an `.ipkg` file.
export covering
build : HasIO io => Path -> Env HasIdris -> EitherT PackErr io ()
build = runIdrisOn "--build"

||| Typecheck a local library given as an `.ipkg` file.
export covering
typecheck : HasIO io => Path -> Env HasIdris -> EitherT PackErr io ()
typecheck = runIdrisOn "--typecheck"

||| Install an Idris application given as a package name
||| or a path to a local `.ipkg` file.
export covering
installApp :  HasIO io
           => Env HasIdris
           -> PkgRep
           -> EitherT PackErr io ()
installApp e n = do
  rp       <- resolve e n
  Just exe <- pure (executable rp) | Nothing => throwE (NoApp n)
  traverse_ (installLib True e) (dependencies rp)
  case rp of
    RGitHub pn url commit ipkg d => do
      False <- executableExists e exe | True => pure ()
      withGit (tmpDir e) url commit $ do
        let pf = patchFile e pn ipkg
        when !(exists pf) (patch ipkg pf)
        idrisPkg e "--build" ipkg
        copyApp e

    RIpkg ipkg d => do
      removeExec e exe
      idrisPkg e "--build" ipkg
      copyApp e

    RLocal _ dir ipkg d => do
      removeExec e exe
      inDir dir $ do
        idrisPkg e "--build" ipkg
        copyApp e
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
      traverse_ (installLib True e) (dependencies rp)
      idrisPkg e "--build" ipkg
      sys "build/exec/\{exe} \{unwords args}"
    _            => do
      installApp e p
      sys "\{show $ packageExec e exe} \{unwords args}"

||| Switch the package collection, installing Idris2 and *pack*
||| if necessary.
export covering
switchCollection : HasIO io => Env HasIdris -> EitherT PackErr io ()
switchCollection e = do
  installApp e "pack"
  link (idrisBinDir e) (packBinDir e)
  link (idrisPrefixDir e) (packIdrisDir e)
  write (e.packDir /> ".db") e.dbVersion.value
