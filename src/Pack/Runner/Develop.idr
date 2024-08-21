module Pack.Runner.Develop

import Data.IORef
import Data.SortedMap
import Idris.Package.Types
import Pack.Config
import Pack.Core
import Pack.Database
import Pack.Runner.Database
import Pack.Runner.Install
import Pack.Runner.Query

%default total

covering
runIdrisOn :
     {auto _ : HasIO io}
  -> {auto _ : IdrisEnv}
  -> (logLevel   : LogLevel)
  -> (cleanBuild : Bool)
  -> (cmd        : CmdArgList)
  -> Desc Safe
  -> EitherT PackErr io ()
runIdrisOn lvl cleanBuild c d = do
  installDeps d
  libPkg [] lvl cleanBuild c d

findIpkg :
     {auto _ : HasIO io}
  -> WithIpkg
  -> Maybe (File Abs)
  -> EitherT PackErr io (Maybe $ File Abs)
findIpkg (Search $ CD dir) fi =
  let searchDir := maybe dir parent fi
   in findInParentDirs isIpkgBody searchDir
findIpkg None         _  = pure Nothing
findIpkg (Use x)      _  = pure (Just x)

-- returns the direct dependencies to be included in a REPL session
covering
replDeps :
     {auto _ : HasIO io}
  -> {auto e : IdrisEnv}
  -> Maybe (Desc Safe)
  -> Autoload
  -> EitherT PackErr io (List PkgName)
replDeps _        (ForcePkgs ps) = pure ps
replDeps (Just d) _              = pure $ dependencies d
replDeps Nothing  NoPkgs         = pure $ []
replDeps Nothing  AutoLibs       = pure $ e.env.config.autoLibs
replDeps Nothing  (AutoPkgs ps)  = pure $ ps
replDeps Nothing  Installed      =
  map name . filter installedLib . snd <$> resolveAll

covering
replOpts :
     {auto _ : HasIO io}
  -> {auto e : IdrisEnv}
  -> (file : Maybe $ File Abs)
  -> EitherT PackErr io (CmdArgList, Codegen, Maybe $ File Abs)
replOpts mf = do
  mp   <- findIpkg e.env.config.withIpkg mf
  for_ mp $ \p => info "Found `.ipkg` file at \{p}"
  md   <- traverse (\p => parseLibIpkg p p) mp
  libs <- map (Library,) <$> replDeps md e.env.config.autoLoad
  tds  <- mapMaybe libName <$> transitiveDeps libs

  let srcDir := maybe [] (\s => ["--source-dir", s]) (md >>= sourcedir . desc)
      pkgs   := concatMap (\td => ["-p", value td]) tds
      cg     := maybe e.env.config.codegen (ipkgCodeGen . desc) md
      cgOpt  := case cg of
                  Default => []
                  _       => ["--cg", cg]
  install libs
  pure (srcDir ++ cgOpt ++ pkgs, cg, mp)

-- return the path of an Idris source file to an `.ipkg` file.
srcFileRelativeToIpkg : (ipkg,idr : Maybe (File Abs)) -> CmdArgList
srcFileRelativeToIpkg _           Nothing    = []
srcFileRelativeToIpkg Nothing     (Just idr) = [ idr ]
srcFileRelativeToIpkg (Just ipkg) (Just idr) =
  let rel := relativeTo ipkg.parent idr.parent
   in [ MkF rel idr.file ]

||| Use the installed Idris to start a REPL session with the
||| given argument string.
export covering
idrisRepl :
     {auto _ : HasIO io}
  -> (file : Maybe $ File Abs)
  -> IdrisEnv
  -> EitherT PackErr io ()
idrisRepl mf e = do
  (opts, _, mp) <- replOpts mf
  env  <- buildEnv

  let arg := srcFileRelativeToIpkg mp mf
      exe := idrisWithCG

  cmd <- case e.env.config.rlwrap of
    UseRlwrap rargs => pure $ ["rlwrap"] ++ rargs ++ exe ++ opts ++ arg
    DoNotUseRlwrap  => pure $ exe ++ opts ++ arg

  case mp of
    Just af => inDir af.parent $ \_ => sysWithEnv cmd env
    Nothing => sysWithEnv cmd env

||| Use the installed Idris to compile the given source file
||| and invoke its main function with the given argument list.
export covering
exec :
     {auto _ : HasIO io}
  -> (file : File Abs)
  -> (args : CmdArgList)
  -> IdrisEnv
  -> EitherT PackErr io ()
exec file args e = do
  (opts, cg, mp) <- replOpts (Just file)
  env  <- buildEnv

  let interp  := case cg of
         Node => ["node"]
         _    => []
      relFile := srcFileRelativeToIpkg mp (Just file)
      exe     := idrisWithCG
      cmd     := exe ++ opts ++ ["-o", e.env.config.output] ++ relFile
      run     := interp ++ ["build/exec/\{e.env.config.output}"] ++ args

  case mp of
    Just af => inDir af.parent $ \_ => do
      sysWithEnvAndLog Build cmd env
      sys run
    Nothing => sysWithEnvAndLog Build cmd env >> sys run

||| Build a local library given as an `.ipkg` file.
export covering %inline
build : HasIO io => PkgOrIpkg -> IdrisEnv -> EitherT PackErr io ()
build f e = findAndParseLocalIpkg f >>= runIdrisOn Build True ["--build"]

||| Install dependencies of a local `.ipkg` file or package name
export covering
buildDeps : HasIO io => PkgOrIpkg -> IdrisEnv -> EitherT PackErr io ()
buildDeps f e = do
  d <- findAndParseLocalIpkg f
  installDeps d

||| Typecheck a local library given as an `.ipkg` file or package name
export covering %inline
typecheck : HasIO io => PkgOrIpkg -> IdrisEnv -> EitherT PackErr io ()
typecheck f e =
  findAndParseLocalIpkg f >>= runIdrisOn Build True ["--typecheck"]

||| Cleanup a local library given as an `.ipkg` file or package name
export covering %inline
clean : HasIO io => PkgOrIpkg -> IdrisEnv -> EitherT PackErr io ()
clean f e = do
  p <- findAndParseLocalIpkg f
  libPkg [] Build False ["--clean"] p
  rmDir (buildPath p)

||| Build and execute a local `.ipkg` file.
export covering
runIpkg :
     {auto _ : HasIO io}
  -> File Abs
  -> (args : CmdArgList)
  -> IdrisEnv
  -> EitherT PackErr io ()
runIpkg p args e = do
  d        <- parseLibIpkg p p
  Just exe <- pure (execPath d) | Nothing => throwE (NoAppIpkg p)
  build (Ipkg p) e
  case ipkgCodeGen d.desc of
    Node => sys $ ["node", exe] ++ args
    _    => sys $ [exe] ++ args

||| Build and execute the test suite of a package.
export covering
runTest :
     {auto _ : HasIO io}
  -> PkgName
  -> (args : CmdArgList)
  -> IdrisEnv
  -> EitherT PackErr io ()
runTest n args e = case lookup n allPackages of
  Nothing                     => throwE (UnknownPkg n)
  Just (Git u c _ _ $ Just t) => do
    d <- withGit n u c pure
    runIpkg (d </> t) args e
  Just (Local d _ _ $ Just t) => runIpkg (d </> t) args e
  Just _                      => do
    warn "No test suite found for \{n}. I'll just typecheck it."
    typecheck (Pkg n) e

||| Install and run an executable given as a package name.
export covering
execApp :
     {auto _ : HasIO io}
  -> PkgName
  -> (args : CmdArgList)
  -> IdrisEnv
  -> EitherT PackErr io ()
execApp p args e = do
  ref <- emptyCache
  ra <- resolveApp p
  install [(App False,p)]
  case ipkgCodeGen ra.desc.desc of
    Node => sys $ ["node", pkgExec ra.name ra.pkg ra.exec] ++ args
    _    => sys $ [pkgExec ra.name ra.pkg ra.exec] ++ args

export covering
runApp :
     {auto _ : HasIO io}
  -> PkgOrIpkg
  -> (args : CmdArgList)
  -> IdrisEnv
  -> EitherT PackErr io ()
runApp (Pkg p)  = execApp p
runApp (Ipkg p) = runIpkg p
