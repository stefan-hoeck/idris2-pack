module Pack.Build

import Control.Monad.State
import Data.List
import Data.List1
import Data.String
import Idris.Package.Types
import Libraries.Data.SortedMap
import Pack.CmdLn
import Pack.Types
import Pack.Err
import Pack.Ipkg
import Pack.Util
import Parser.Package
import System.Directory
import System

%default total

||| The directory where Idris2, installed libraries,
||| and binaries will be installed.
|||
||| This corresponds to "$IDRIS2_PREFIX".
export
idrisPrefixDir : Env -> String
idrisPrefixDir env = "\{env.conf.packDir}/\{env.conf.dbVersion}"

||| The directory where Idris2 packages will be installed.
export
idrisInstallDir : Env -> String
idrisInstallDir env = "\{idrisPrefixDir env}/idris2-\{env.db.idrisVersion}"

||| The directory where binaries will be installed.
export
idrisBinDir : Env -> String
idrisBinDir env = "\{idrisPrefixDir env}/bin"

||| A symbolic link to `idrisBinDir` of the current
||| db version. This corresponds to `$PACK_DIR/bin`
||| and should be added to the `$PATH` variable in
||| order to have access to the current Idris2 binary
||| and related applications.
export
packBinDir : Config -> String
packBinDir c = "\{c.packDir}/bin"

||| A symbolic to `idrisInstallDir` of the current
||| db version. Let `$IDRIS2_PREFIX` point to this
||| directory.
export
packIdrisDir : Config -> String
packIdrisDir c = "\{c.packDir}/idris2"

||| Location of the Idris2 executable used to build
||| packages.
export
idrisExec : Env -> String
idrisExec env = "\{idrisBinDir env}/idris2"

||| Location of an executable of the given name.
export
packageExec : Env -> String -> String
packageExec env n = "\{idrisBinDir env}/\{n}"

||| `$PREFIX` variable during Idris2 installation
export
prefixVar : Env -> String
prefixVar env = "PREFIX=\"\{idrisPrefixDir env}\""

||| `$PREFIX` variable during Idris2 installation
export
idrisBootVar : Env -> String
idrisBootVar env = "IDRIS2_BOOT=\"\{idrisExec env}\""

||| `$PREFIX` variable during Idris2 installation
export
schemeVar : Env -> String
schemeVar env = "SCHEME=\"\{env.conf.scheme}\""

||| Returns the directory where a package for the current
||| package collection is installed.
export
packageInstallDir : Env -> ResolvedPackage -> String
packageInstallDir e p =
  let vers = e.db.idrisVersion
      dir  = idrisInstallDir e
   in case p of
        Base     => "\{dir}/base-\{vers}"
        Contrib  => "\{dir}/contrib-\{vers}"
        Linear   => "\{dir}/linear-\{vers}"
        Network  => "\{dir}/network-\{vers}"
        Prelude  => "\{dir}/prelude-\{vers}"
        Idris2   => "\{dir}/idris2-\{vers}"
        Test     => "\{dir}/test-\{vers}"
        RP _ _ _ _ d   =>
          let v = maybe "0" show d.version
           in "\{dir}/\{d.name}-\{v}"
        Ipkg p d =>
          let v = maybe "0" show d.version
           in "\{dir}/\{d.name}-\{v}"
        Local _ _ _ d =>
          let v = maybe "0" show d.version
           in "\{dir}/\{d.name}-\{v}"

||| Builds and installs the Idris commit given in the environment.
export
mkIdris : HasIO io => Env -> EitherT PackErr io ()
mkIdris env = do
  False <- exists (idrisInstallDir env) | True => pure ()

  withGit env.conf idrisRepo env.db.idrisCommit $ do
    sys "make bootstrap \{prefixVar env} \{schemeVar env}"
    sys "make install \{prefixVar env}"
    sys "make clean"
    sys "make all \{idrisBootVar env} \{prefixVar env}"
    sys "make install \{idrisBootVar env} \{prefixVar env}"
    sys "make install-with-src-libs \{idrisBootVar env} \{prefixVar env}"
    sys "make install-with-src-api \{idrisBootVar env} \{prefixVar env}"

-- test if a file path ends on `.ipkg`.
isIpkgPath : String -> Bool
isIpkgPath x = case reverse $ forget $ split ('.' ==) x of
  "ipkg" :: _ => True
  _           => False

||| Lookup a package name in the package data base,
||| then download and extract its `.ipkg` file from
||| its GitHub repository.
export covering
resolve :  HasIO io
        => (env : Env)
        -> String
        -> EitherT PackErr io ResolvedPackage
resolve env "base"    = pure Base
resolve env "contrib" = pure Contrib
resolve env "linear"  = pure Linear
resolve env "idris2"  = pure Idris2
resolve env "network" = pure Network
resolve env "prelude" = pure Prelude
resolve env "test"    = pure Test
resolve env n         = case lookup n env.db.packages of
  -- package is not in the database, so we check if we
  -- are dealing with an `.ipkg` file instead
  Nothing  =>
    if isIpkgPath n
       then do
         (nm,flds) <- parseFile n (parsePkgDesc n)
         pure $ Ipkg n (addFields nm flds)
       else throwE (UnknownPkg n)

  -- this is a known package so we download its `.ipkg`
  -- file from GitHub. TODO: We should probably cache these
  -- to speed things up a bit
  Just (MkPackage n url (Just commit) ipkg) => withGit env.conf url commit $ do
    (nm,flds) <- parseFile ipkg (parsePkgDesc ipkg)
    pure $ RP n url commit ipkg (addFields nm flds)
  Just (MkPackage n dir Nothing ipkg) => inDir dir $ do
    (nm,flds) <- parseFile ipkg (parsePkgDesc ipkg)
    pure $ Local n dir ipkg (addFields nm flds)

-- check if a package has already been built and installed
packageExists : HasIO io => (env : Env) -> ResolvedPackage -> EitherT PackErr io Bool
packageExists env p = exists (packageInstallDir env p)

-- check if the executable of a package has already been
-- built and installed
executableExists : HasIO io => (env : Env) -> String -> EitherT PackErr io Bool
executableExists env p = exists (packageExec env p)

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
  mkIdris e
  rp <- resolve e n
  case rp of
    RP _ url commit ipkg d => do
      False <- packageExists e rp | True => pure ()
      traverse_ (installLib ws e) (map pkgname d.depends)
      withGit e.conf url commit $ do
        sys "\{idrisExec e} \{installCmd ws} \{ipkg}"
    Ipkg ipkg d => do
      traverse_ (installLib ws e) (map pkgname d.depends)
      sys "\{idrisExec e} \{installCmd ws} \{ipkg}"
    Local _ dir ipkg d => do
      traverse_ (installLib ws e) (map pkgname d.depends)
      inDir dir $ sys "\{idrisExec e} \{installCmd ws} \{ipkg}"
    _             => do
      False <- packageExists e rp | True => pure ()
      throwE (MissingCorePackage n e.db.idrisVersion e.db.idrisCommit)

removeExec : HasIO io => Env -> String -> EitherT PackErr io ()
removeExec env n = do
  rmFile (packageExec env n)
  rmDir "\{packageExec env n}_app"

export covering
remove : HasIO io => Env -> String -> EitherT PackErr io ()
remove env n = do
  res <- resolve env n
  rmDir (packageInstallDir env res)
  maybe (pure ()) (removeExec env) $ executable res

covering
installApp : HasIO io => Env -> String -> EitherT PackErr io ()
installApp env n = do
  mkIdris env
  res <- resolve env n
  case res of
    RP _ url commit ipkg d => case d.executable of
      Nothing => throwE (NoApp n)
      Just e  => do
        False <- executableExists env e | True => pure ()
        traverse_ (installLib True env) (map pkgname d.depends)
        withGit env.conf url commit $ do
          sys "\{idrisExec env} --build \{ipkg}"
          sys "cp -r build/exec/* \{idrisBinDir env}"

    Ipkg ipkg d => case d.executable of
      Nothing => throwE (NoApp n)
      Just e  => do
        removeExec env e
        traverse_ (installLib True env) (map pkgname d.depends)
        sys "\{idrisExec env} --build \{ipkg}"
        sys "cp -r build/exec/* \{idrisBinDir env}"

    Local _ dir ipkg d => case d.executable of
      Nothing => throwE (NoApp n)
      Just e  => do
        removeExec env e
        traverse_ (installLib True env) (map pkgname d.depends)
        inDir dir $ do
          sys "\{idrisExec env} --build \{ipkg}"
          sys "cp -r build/exec/* \{idrisBinDir env}"
    _ => throwE (NoApp n)

covering
build : HasIO io => String -> Env -> EitherT PackErr io ()
build p env = do
  Ipkg ipkg d <- resolve env p | _ => throwE BuildMany
  traverse_ (installLib True env) (map pkgname d.depends)
  sys "\{idrisExec env} --build \{ipkg}"

toState : HasIO io => EitherT err io a -> StateT s io (Either err a)
toState = lift . runEitherT

updateRep :  HasIO io
          => String
          -> Report
          -> StateT ReportDB io Report
updateRep p rep = modify (insert p rep) $> rep

updateFail :  HasIO io
           => String
           -> ResolvedPackage
           -> List String
           -> StateT ReportDB io Report
updateFail n rp rs = updateRep n (Failure rp rs)

updateErr :  HasIO io
          => String
          -> PackErr
          -> StateT ReportDB io Report
updateErr n err = updateRep n (Error n err)

updateSuccess :  HasIO io
              => String
              -> ResolvedPackage
              -> StateT ReportDB io Report
updateSuccess n pkg = updateRep n (Success pkg)

covering
checkPkg : HasIO io => Env -> String -> StateT ReportDB io Report
checkPkg env p = do
  Nothing <- lookup p <$> get | Just rep => pure rep
  Right resPkg  <- toState $ resolve env p
    | Left err => updateErr p err 
  case resPkg of
    RP _ url commit ipkg d => do
      [] <- failingDependencies <$> traverse (checkPkg env) (map pkgname d.depends)
        | rs => updateFail p resPkg rs
      res <- toState $ withGit env.conf url commit $ do
        sys "\{idrisExec env} --install \{ipkg}"
      case res of
        Left  _ => updateFail p resPkg []
        Right _ => updateSuccess p resPkg

    Ipkg ipkg d => do
      [] <- failingDependencies <$> traverse (checkPkg env) (map pkgname d.depends)
        | rs => updateFail p resPkg rs
      res <- toState $ sys "\{idrisExec env} --install \{ipkg}"
      case res of
        Left  _ => updateFail p resPkg []
        Right _ => updateSuccess p resPkg

    Local _ dir ipkg d => do
      [] <- failingDependencies <$> traverse (checkPkg env) (map pkgname d.depends)
        | rs => updateFail p resPkg rs
      res <- toState $ inDir dir $
        sys "\{idrisExec env} --install \{ipkg}"
      case res of
        Left  _ => updateFail p resPkg []
        Right _ => updateSuccess p resPkg

    _ => updateSuccess p resPkg

covering
checkDB : HasIO io => Env -> EitherT PackErr io ()
checkDB env = do
  mkIdris env
  rep <- liftIO $ execStateT empty 
                $ traverse_ (checkPkg env . name) env.db.packages
  putStrLn $ printReport rep

covering
typecheck : HasIO io => String -> Env -> EitherT PackErr io ()
typecheck p env = do
  Ipkg ipkg d <- resolve env p | _ => throwE BuildMany
  traverse_ (installLib True env) (map pkgname d.depends)
  sys "\{idrisExec env} --typecheck \{ipkg}"

covering
execApp : HasIO io => String -> List String -> Env -> EitherT PackErr io ()
execApp p args env = do
    res <- resolve env p
    case res of
      RP _ _ _ _ d => case d.executable of
        Nothing => throwE (NoApp p)
        Just e  => do
          installApp env p
          sys "\{packageExec env e} \{unwords args}"
      Local _ _ _ d => case d.executable of
        Nothing => throwE (NoApp p)
        Just e  => do
          installApp env p
          sys "\{packageExec env e} \{unwords args}"
      Ipkg ipkg d => case d.executable of
        Nothing => throwE (NoApp p)
        Just e  => do
          traverse_ (installLib True env) (map pkgname d.depends)
          sys "\{idrisExec env} --build \{ipkg}"
          sys "build/exec/\{e} \{unwords args}"
      _ => throwE (NoApp p)

covering
switchTo : HasIO io => Config -> EitherT PackErr io ()
switchTo c = do
  e <- env c
  mkIdris e
  installApp e "pack"
  rmFile (packBinDir c)
  rmFile (packIdrisDir c)
  sys "ln -s \{idrisBinDir e} \{packBinDir c}"
  sys "ln -s \{idrisPrefixDir e} \{packIdrisDir c}"
  write "\{c.packDir}/.db" e.conf.dbVersion

export covering
runCmd : HasIO io => EitherT PackErr io ()
runCmd = do
  c <- getConfig
  case c.cmd of
    UpdateDB          => updateDB c
    Exec p args       => env c >>= execApp p args
    Build p           => env c >>= build p
    Typecheck p       => env c >>= typecheck p
    CheckDB _         => env c >>= checkDB
    PrintHelp         => putStrLn usageInfo
    Install ps        => env c >>= \e => traverse_ (installLib False e) ps
    Remove ps         => env c >>= \e => traverse_ (remove e) ps
    InstallWithSrc ps => env c >>= \e => traverse_ (installLib True e) ps
    InstallApp ps     => env c >>= \e => traverse_ (installApp e) ps
    SwitchRepo _      => switchTo c
    FromHEAD p        => env c >>= fromHEAD p
