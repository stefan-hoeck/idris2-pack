module Pack.Build

import Data.List
import Data.List1
import Data.String
import Idris.Package.Types
import Pack.CmdLn
import Pack.Types
import Pack.Err
import Pack.Ipkg
import Pack.Util
import Parser.Package
import System.Directory
import System

%default total

export
commit : (env : Env) => String
commit = env.db.idrisCommit

export
version : (env : Env) => String
version = env.db.idrisVersion

export
idrisPrefixDir : (env : Env) => String
idrisPrefixDir = "\{rootDir}/\{env.conf.dbVersion}"

export
idrisInstallDir : (env : Env) => String
idrisInstallDir = "\{idrisPrefixDir}/idris2-\{env.db.idrisVersion}"

export
idrisBinDir : Env => String
idrisBinDir = "\{idrisPrefixDir}/bin"

export
packBinDir : Env => String
packBinDir = "\{rootDir}/bin"

export
packIdrisDir : Env => String
packIdrisDir = "\{rootDir}/idris2"

export
idrisExec : Env => String
idrisExec = "\{idrisBinDir}/idris2"

export
packageExec : Env => String -> String
packageExec n = "\{idrisBinDir}/\{n}"

export
prefixVar : Env => String
prefixVar = "PREFIX=\"\{idrisPrefixDir}\""

export
idrisPrefixVar : Env => String
idrisPrefixVar = "IDRIS2_PREFIX=\"\{idrisPrefixDir}\""

export
idrisExecWithPrefix : Env => String
idrisExecWithPrefix = "\{idrisPrefixVar} \{idrisExec}"

export
packageInstallDir : Env => ResolvedPackage -> String
packageInstallDir Base     = "\{idrisInstallDir}/base-\{Build.version}"
packageInstallDir Contrib  = "\{idrisInstallDir}/contrib-\{Build.version}"
packageInstallDir Linear   = "\{idrisInstallDir}/linear-\{Build.version}"
packageInstallDir Network  = "\{idrisInstallDir}/network-\{Build.version}"
packageInstallDir Prelude  = "\{idrisInstallDir}/prelude-\{Build.version}"
packageInstallDir Idris2   = "\{idrisInstallDir}/idris2-\{Build.version}"
packageInstallDir Test     = "\{idrisInstallDir}/test-\{Build.version}"
packageInstallDir (RP p d) = 
    let v = maybe "0" show d.version
     in "\{idrisInstallDir}/\{d.name}-\{v}"
packageInstallDir (Local p d) = 
    let v = maybe "0" show d.version
     in "\{idrisInstallDir}/\{d.name}-\{v}"

||| Builds and installs the Idris commit given in the environment.
export
mkIdris : HasIO io => Env => EitherT PackErr io ()
mkIdris = do
  False <- exists idrisInstallDir | True => pure ()

  withGit "https://github.com/idris-lang/Idris2.git" commit $ do
    putStrLn "Build project"
    sys "make all"
    putStrLn "Install project"
    sys "make install \{prefixVar} \{idrisPrefixVar}"
    putStrLn "Cleanup"
    sys "make clean"
    putStrLn "Install API"
    sys "make install-api \{idrisPrefixVar}"

isIpkgPath : String -> Bool
isIpkgPath x = case reverse $ forget $ split ('.' ==) x of
  "ipkg" :: _ => True
  _           => False

export covering
resolve :  HasIO io
        => (env : Env)
        => String
        -> EitherT PackErr io ResolvedPackage
resolve "base"    = pure Base
resolve "contrib" = pure Contrib
resolve "linear"  = pure Linear
resolve "idris2"  = pure Idris2
resolve "network" = pure Network
resolve "prelude" = pure Prelude
resolve "test"    = pure Test
resolve n         = case find ((n ==) . name) env.db.packages of
  Nothing  =>
    if isIpkgPath n
       then do
         (nm,flds) <- parseFile n (parsePkgDesc n)
         pure $ Local n (addFields nm flds)
       else throwE (UnknownPkg n)
  Just pkg => withGit pkg.url pkg.commit $ do
    (nm,flds) <- parseFile pkg.ipkg (parsePkgDesc pkg.ipkg)
    pure $ RP pkg (addFields nm flds)

packageExists : HasIO io => (env : Env) => ResolvedPackage -> EitherT PackErr io Bool
packageExists p = exists (packageInstallDir p)

executableExists : HasIO io => (env : Env) => String -> EitherT PackErr io Bool
executableExists p = exists (packageExec p)

installCmd : (withSrc : Bool) -> String
installCmd True  = "--install-with-src"
installCmd False = "--install"

export covering
installLib :  HasIO io
           => (withSrc : Bool)
           -> Env
           -> String
           -> EitherT PackErr io ()
installLib ws e n = do
  mkIdris
  putStrLn "Trying to install \{n}"
  rp <- resolve n
  False <- packageExists rp | True => putStrLn "Package already exists"
  case rp of
    RP pkg d => do
      traverse_ (installLib ws e) (map pkgname d.depends)
      withGit pkg.url pkg.commit $ do
        sys "\{idrisExecWithPrefix} \{installCmd ws} \{pkg.ipkg}"
    Local ipkg d => do
      traverse_ (installLib ws e) (map pkgname d.depends)
      sys "\{idrisExecWithPrefix} \{installCmd ws} \{ipkg}"
    _             => throwE (MissingCorePackage n Build.version Build.commit)

covering
installApp : HasIO io => Env -> String -> EitherT PackErr io ()
installApp env n = do
  mkIdris
  putStrLn "Trying to install \{n}"
  res <- resolve n
  case res of
    RP pkg d => case d.executable of
      Nothing => throwE (NoApp n)
      Just e  => do
        False <- executableExists e | True => putStrLn "Executable exists"
        traverse_ (installLib True env) (map pkgname d.depends)
        withGit pkg.url pkg.commit $ do
          sys "\{idrisExecWithPrefix} --build \{pkg.ipkg}"
          sys "cp -r build/exec/* \{idrisBinDir}"
    Local ipkg d => case d.executable of
      Nothing => throwE (NoApp n)
      Just e  => do
        False <- executableExists e | True => putStrLn "Executable exists"
        traverse_ (installLib True env) (map pkgname d.depends)
        sys "\{idrisExecWithPrefix} --build \{ipkg}"
        sys "cp -r build/exec/* \{idrisBinDir}"
    _ => throwE (NoApp n)

covering
switchTo : HasIO io => Config -> EitherT PackErr io ()
switchTo c = do
  e <- env
  mkIdris
  rmFile packBinDir
  rmFile packIdrisDir
  sys "ln -s \{idrisBinDir} \{packBinDir}"
  sys "ln -s \{idrisPrefixDir} \{packIdrisDir}"
  installApp e "pack"
  write "\{rootDir}/.db" e.conf.dbVersion

export covering
runCmd : HasIO io => EitherT PackErr io ()
runCmd = do
  c <- getConfig
  case c.cmd of
    UpdateDB       => updateDB
    PrintHelp      => putStrLn usageInfo
    Install        => env >>= \e => traverse_ (installLib False e) c.packages
    InstallWithSrc => env >>= \e => traverse_ (installLib True e) c.packages
    InstallApp     => env >>= \e => traverse_ (installApp e) c.packages
    SwitchRepo     => switchTo c
