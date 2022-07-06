module Pack.Runner.Develop

import Idris.Package.Types
import Pack.Config.Env
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types
import Pack.Runner.Database
import Pack.Runner.Install

%default total

covering
runIdrisOn :  HasIO io
           => (cmd : String)
           -> Env HasIdris
           -> Desc Safe
           -> EitherT PackErr io ()
runIdrisOn c e d = do
  installDeps e d
  libPkg e [] c d

findIpkg :  HasIO io
         => WithIpkg
         -> Maybe (File Abs)
         -> EitherT PackErr io (Maybe $ File Abs)
findIpkg (Search dir) fi =
  let searchDir := maybe dir parent fi
   in findInParentDirs isIpkgBody searchDir
findIpkg None         _  = pure Nothing
findIpkg (Use x)      _  = pure (Just x)

covering
replOpts :  HasIO io
         => Env HasIdris
         -> (file : Maybe $ File Abs)
         -> EitherT PackErr io (String, Maybe $ File Abs)
replOpts e mf = do
  Just p <- findIpkg e.withIpkg mf | Nothing => pure ("",Nothing)
  info e "Found `.ipkg` file at \{p}"
  d <- parseLibIpkg e p p
  let srcDir := maybe "" (\s => "--source-dir \"\{s}\"") d.desc.sourcedir
      pkgs   := unwords $ map (("-p " ++) . value) (dependencies d)
  installDeps e d
  pure ("\{srcDir} \{pkgs}", Just p)


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
          -> Env HasIdris
          -> EitherT PackErr io ()
idrisRepl mf e = do
  pth  <- packagePath e
  (opts, mp) <- replOpts e mf

  let args := srcFileRelativeToIpkg mp mf
      exe  := idrisWithCG e

  cmd <- case e.rlwrap of
    True  => pure "rlwrap \{exe} \{opts} \{args}"
    False => pure "\{exe} \{opts} \{args}"

  case mp of
    Just af => inDir af.parent $ \_ => sysWithEnv cmd [pth]
    Nothing => sysWithEnv cmd [pth]

||| Use the installed Idris to compile the given source file
||| and invoke its main function with the given argument list.
export covering
exec :  HasIO io
     => (file : File Abs)
     -> (args : List String)
     -> Env HasIdris
     -> EitherT PackErr io ()
exec file args e = do
  pth  <- packagePath e
  (opts, mp) <- replOpts e (Just file)

  let relFile := srcFileRelativeToIpkg mp (Just file)
      exe     := idrisWithCG e
      cmd     := "\{exe} \{opts} -o \{e.output} \{relFile}"
      run     := "build/exec/\{e.output} \{unwords args}"

  case mp of
    Just af => inDir af.parent $ \_ => do
      sysWithEnv cmd [pth]
      sys run
    Nothing => sysWithEnv cmd [pth] >> sys run

||| Build a local library given as an `.ipkg` file.
export covering %inline
build : HasIO io => File Abs -> Env HasIdris -> EitherT PackErr io ()
build f e = parseLibIpkg e f f >>= runIdrisOn "--build" e

||| Install dependencies of a local `.ipkg` file
export covering
buildDeps : HasIO io => File Abs -> Env HasIdris -> EitherT PackErr io ()
buildDeps f e = do
  d <- parseLibIpkg e f f
  installDeps e d

||| Typecheck a local library given as an `.ipkg` file.
export covering %inline
typecheck : HasIO io => File Abs -> Env HasIdris -> EitherT PackErr io ()
typecheck f e = parseLibIpkg e f f >>= runIdrisOn "--typecheck" e

||| Build and execute a local `.ipkg` file.
export covering
runIpkg :  HasIO io
        => File Abs
        -> (args : List String)
        -> Env HasIdris
        -> EitherT PackErr io ()
runIpkg p args e = do
  d        <- parseLibIpkg e p p
  Just exe <- pure (execPath d) | Nothing => throwE (NoAppIpkg p)
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
  ra <- resolveApp e p
  case ra.pkg of
    GitHub {}      => do
      install e [(Bin,p)]
      sys "\{pkgExec e ra.name ra.pkg ra.exec} \{unwords args}"
    Local d ipkg _ => runIpkg (toAbsFile d ipkg) args e
    Core {}        => throwE (NoApp p)
