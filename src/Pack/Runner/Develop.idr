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
           -> File Abs
           -> Env HasIdris
           -> EitherT PackErr io ()
runIdrisOn c p e = parseIpkgFile p id >>= idrisPkg e [] c p . snd

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
  (_,desc) <- parseIpkgFile p id
  traverse_ (installLib e) (dependencies desc)
  let srcDir = maybe "" (\s => "--source-dir \"\{s}\"") desc.sourcedir
      pkgs = unwords $ map (("-p " ++) . pkgname) desc.depends
  pure ("\{srcDir} \{pkgs}", Just p)

||| Use the installed Idris to start a REPL session with the
||| given argument string.
export covering
idrisRepl :  HasIO io
          => (file : Maybe $ File Abs)
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
export covering %inline
build : HasIO io => File Abs -> Env HasIdris -> EitherT PackErr io ()
build = runIdrisOn "--build"

||| Install dependencies of a local `.ipkg` file
export covering
buildDeps : HasIO io => File Abs -> Env HasIdris -> EitherT PackErr io ()
buildDeps ipkg e = do
  (_,d) <- parseIpkgFile ipkg id
  traverse_ (installLib e) (dependencies d)

||| Typecheck a local library given as an `.ipkg` file.
export covering %inline
typecheck : HasIO io => File Abs -> Env HasIdris -> EitherT PackErr io ()
typecheck = runIdrisOn "--typecheck"

||| Install and run an executable given as a package name.
export covering
runIpkg :  HasIO io
        => File Abs
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
