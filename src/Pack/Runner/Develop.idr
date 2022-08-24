module Pack.Runner.Develop

import Data.IORef
import Idris.Package.Types
import Pack.Config
import Pack.Core
import Pack.Database
import Pack.Runner.Database
import Pack.Runner.Install

%default total

covering
runIdrisOn :  HasIO io
           => IdrisEnv
           => (cmd : String)
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

covering
replOpts :  HasIO io
         => (e : IdrisEnv)
         => (file : Maybe $ File Abs)
         -> EitherT PackErr io (String, Maybe $ File Abs)
replOpts mf = do
  Just p <- findIpkg e.env.config.withIpkg mf | Nothing => pure ("",Nothing)
  info "Found `.ipkg` file at \{p}"
  d <- parseLibIpkg p p
  let srcDir := maybe "" (\s => "--source-dir \"\{s}\"") d.desc.sourcedir
      pkgs   := unwords $ map (("-p " ++) . value) (dependencies d)
  installDeps d
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
          -> IdrisEnv
          -> EitherT PackErr io ()
idrisRepl mf e = do
  pth  <- packagePath
  (opts, mp) <- replOpts mf

  let args := srcFileRelativeToIpkg mp mf
      exe  := idrisWithCG

  cmd <- case e.env.config.rlwrap of
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
     -> IdrisEnv
     -> EitherT PackErr io ()
exec file args e = do
  pth  <- packagePath
  (opts, mp) <- replOpts (Just file)


  let interp = case e.env.config.codegen of
                  Node => "node "
                  _ =>  ""
      relFile := srcFileRelativeToIpkg mp (Just file)
      exe     := idrisWithCG
      cmd     := "\{exe} \{opts} -o \{e.env.config.output} \{relFile}"
      run     := "\{interp} build/exec/\{e.env.config.output} \{unwords args}"

  case mp of
    Just af => inDir af.parent $ \_ => do
      sysWithEnv cmd [pth]
      sys run
    Nothing => sysWithEnv cmd [pth] >> sys run

||| Build a local library given as an `.ipkg` file.
export covering %inline
build :  HasIO io
      => Either (File Abs) PkgName
      -> IdrisEnv
      -> EitherT PackErr io ()
build f e = findAndParseLocalIpkg f >>= runIdrisOn "--build"

||| Install dependencies of a local `.ipkg` file
export covering
buildDeps :  HasIO io
          => Either (File Abs) PkgName
          -> IdrisEnv
          -> EitherT PackErr io ()
buildDeps f e = do
  d <- findAndParseLocalIpkg f
  installDeps d

||| Typecheck a local library given as an `.ipkg` file.
export covering %inline
typecheck :  HasIO io
          => Either (File Abs) PkgName
          -> IdrisEnv
          -> EitherT PackErr io ()
typecheck f e = findAndParseLocalIpkg f >>= runIdrisOn "--typecheck"

||| Build and execute a local `.ipkg` file.
export covering
runIpkg :  HasIO io
        => File Abs
        -> (args : List String)
        -> IdrisEnv
        -> EitherT PackErr io ()
runIpkg p args e = do
  d        <- parseLibIpkg p p
  Just exe <- pure (execPath d) | Nothing => throwE (NoAppIpkg p)
  build (Left p) e
  case e.env.config.codegen of
    Node => sys "node \{exe} \{unwords args}"
    _ =>  sys "\{exe} \{unwords args}"


||| Install and run an executable given as a package name.
export covering
execApp :  HasIO io
        => PkgName
        -> (args : List String)
        -> IdrisEnv
        -> EitherT PackErr io ()
execApp p args e = do
  ref <- emptyCache
  ra <- resolveApp p
  install [(App False,p)]
  case e.env.config.codegen of
    Node => sys "node \{pkgExec ra.name ra.pkg ra.exec} \{unwords args}"
    _ => sys "\{pkgExec ra.name ra.pkg ra.exec} \{unwords args}"
