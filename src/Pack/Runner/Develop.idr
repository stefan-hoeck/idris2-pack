module Pack.Runner.Develop

import Data.IORef
import Idris.Package.Types
import Pack.Config
import Pack.Core
import Pack.Database
import Pack.Runner.Database
import Pack.Runner.Install
import Pack.Runner.Query

%default total

covering
runIdrisOn :  HasIO io
           => IdrisEnv
           => (cmd : CmdArgList)
           -> Desc Safe
           -> EitherT PackErr io ()
runIdrisOn c d = do
  installDeps d
  libPkg [] c d

findIpkg :  HasIO io
         => WithIpkg
         -> Maybe (File Abs)
         -> EitherT PackErr io (Maybe $ File Abs)
findIpkg (Search $ CD dir) fi =
  let searchDir := maybe dir parent fi
   in findInParentDirs isIpkgBody searchDir
findIpkg None         _  = pure Nothing
findIpkg (Use x)      _  = pure (Just x)

-- returns the direct dependencies to be included in a REPL session
covering
replDeps :  HasIO io
         => (e : IdrisEnv)
         => Maybe (Desc Safe)
         -> Autoload
         -> EitherT PackErr io (List PkgName)
replDeps _        (ForcePkgs ps) = pure ps
replDeps (Just d) _              = pure $ dependencies d
replDeps Nothing  NoPkgs         = pure $ []
replDeps Nothing  AutoLibs       = pure $ e.env.config.autoLibs
replDeps Nothing  (AutoPkgs ps)  = pure $ ps
replDeps Nothing  Installed      =
  map name . filter installedLib <$> resolveAll

covering
replOpts :  HasIO io
         => (e : IdrisEnv)
         => (file : Maybe $ File Abs)
         -> EitherT PackErr io (CmdArgList, Codegen, Maybe $ File Abs)
replOpts mf = do
  mp   <- findIpkg e.env.config.withIpkg mf
  for_ mp $ \p => info "Found `.ipkg` file at \{p}"
  md   <- traverse (\p => parseLibIpkg p p) mp
  libs <- map (Library,) <$> replDeps md e.env.config.autoLoad
  tds  <- mapMaybe libName <$> transitiveDeps libs

  let srcDir := maybe [] (\s => ["--source-dir", "\{s}"]) (md >>= sourcedir . desc)
      pkgs   := concatMap (\td => ["-p", value td]) tds
      cg     := maybe e.env.config.codegen (ipkgCodeGen . desc) md
      cgOpt  := case cg of
                  Default => []
                  _       => ["--cg", "\{cg}"]
  install libs
  pure (srcDir ++ cgOpt ++ pkgs, cg, mp)

-- return the path of an Idris source file to an `.ipkg` file.
srcFileRelativeToIpkg : (ipkg,idr : Maybe (File Abs)) -> String
srcFileRelativeToIpkg _           Nothing    = ""
srcFileRelativeToIpkg Nothing     (Just idr) = "\{idr}"
srcFileRelativeToIpkg (Just ipkg) (Just idr) =
  let rel := relativeTo ipkg.parent idr.parent
   in "\{MkF rel idr.file}"

||| Use the installed Idris to start a REPL session with the
||| given argument string.
export covering
idrisRepl :  HasIO io
          => (file : Maybe $ File Abs)
          -> IdrisEnv
          -> EitherT PackErr io ()
idrisRepl mf e = do
  (opts, _, mp) <- replOpts mf
  pth  <- packagePath

  let arg := srcFileRelativeToIpkg mp mf
      exe := idrisWithCG

  cmd <- case e.env.config.rlwrap of
    True  => pure $ ["rlwrap"] ++ exe ++ opts ++ [arg]
    False => pure $ exe ++ opts ++ [arg]

  case mp of
    Just af => inDir af.parent $ \_ => sysWithEnv cmd [pth]
    Nothing => sysWithEnv cmd [pth]

||| Use the installed Idris to compile the given source file
||| and invoke its main function with the given argument list.
export covering
exec :  HasIO io
     => (file : File Abs)
     -> (args : CmdArgList)
     -> IdrisEnv
     -> EitherT PackErr io ()
exec file args e = do
  (opts, cg, mp) <- replOpts (Just file)
  pth  <- packagePath


  let interp = case cg of
                  Node => ["node"]
                  _    => []
      relFile := srcFileRelativeToIpkg mp (Just file)
      exe     := idrisWithCG
      cmd     := exe ++ opts ++ ["-o", "\{e.env.config.output}", "\{relFile}"]
      run     := interp ++ ["build/exec/\{e.env.config.output}"] ++ args

  case mp of
    Just af => inDir af.parent $ \_ => do
      sysWithEnvAndLog Build cmd [pth]
      sys run
    Nothing => sysWithEnvAndLog Build cmd [pth] >> sys run

||| Build a local library given as an `.ipkg` file.
export covering %inline
build :  HasIO io
      => Either (File Abs) PkgName
      -> IdrisEnv
      -> EitherT PackErr io ()
build f e = findAndParseLocalIpkg f >>= runIdrisOn ["--build"]

||| Install dependencies of a local `.ipkg` file or package name
export covering
buildDeps :  HasIO io
          => Either (File Abs) PkgName
          -> IdrisEnv
          -> EitherT PackErr io ()
buildDeps f e = do
  d <- findAndParseLocalIpkg f
  installDeps d

||| Typecheck a local library given as an `.ipkg` file or package name
export covering %inline
typecheck :  HasIO io
          => Either (File Abs) PkgName
          -> IdrisEnv
          -> EitherT PackErr io ()
typecheck f e = findAndParseLocalIpkg f >>= runIdrisOn ["--typecheck"]

||| Cleanup a local library given as an `.ipkg` file or package name
export covering %inline
clean :  HasIO io
          => Either (File Abs) PkgName
          -> IdrisEnv
          -> EitherT PackErr io ()
clean f e = findAndParseLocalIpkg f >>= libPkg [] ["--clean"]

||| Build and execute a local `.ipkg` file.
export covering
runIpkg :  HasIO io
        => File Abs
        -> (args : CmdArgList)
        -> IdrisEnv
        -> EitherT PackErr io ()
runIpkg p args e = do
  d        <- parseLibIpkg p p
  Just exe <- pure (execPath d) | Nothing => throwE (NoAppIpkg p)
  build (Left p) e
  case ipkgCodeGen d.desc of
    Node => sys $ ["node", "\{exe}"] ++ args
    _    => sys $ ["\{exe}"] ++ args

||| Install and run an executable given as a package name.
export covering
execApp :  HasIO io
        => PkgName
        -> (args : CmdArgList)
        -> IdrisEnv
        -> EitherT PackErr io ()
execApp p args e = do
  ref <- emptyCache
  ra <- resolveApp p
  install [(App False,p)]
  case ipkgCodeGen ra.desc.desc of
    Node => sys $ ["node", "\{pkgExec ra.name ra.pkg ra.exec}"] ++ args
    _ => sys $ ["\{pkgExec ra.name ra.pkg ra.exec}"] ++ args
