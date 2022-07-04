module Pack.Runner.Query

import Core.Name.Namespace
import Data.List
import Data.Maybe
import Data.SortedMap
import Idris.Package.Types
import Pack.CmdLn.Types
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types
import Pack.Runner.Database
import Pack.Runner.Install

%default total

pkgNames : Env s -> List PkgName
pkgNames e = keys (allPackages e)

query_ :  HasIO io
       => (e : Env s)
       -> (q : ResolvedLib () -> Maybe b)
       -> EitherT PackErr io (List b)
query_ e q = mapMaybe id <$> traverse run (pkgNames e)
  where run : PkgName -> EitherT PackErr io (Maybe b)
        run n = q <$> resolveLib e n

shortDesc : ResolvedLib t -> Maybe String
shortDesc = brief . desc . desc

deps : ResolvedLib t -> List PkgName
deps = dependencies

modules : ResolvedLib t -> List String
modules = map (show . fst) . modules . desc . desc

prettyDeps : ResolvedLib t -> List String
prettyDeps rl = case deps rl of
  []     => ["Dependencies :"]
  h :: t => "Dependencies : \{h}" :: map (indent 15 . interpolate) t

prettyModules : String -> ResolvedLib t -> List String
prettyModules s rl = case filter (isInfixOf s) (modules rl) of
  []     => ["Modules :"]
  h :: t => "Modules : \{h}" :: map (indent 10) t

details : ResolvedLib t -> List String
details rl = case rl.pkg of
  GitHub url commit ipkg _ => [
    "Type         : GitHub project"
  , "URL          : \{url}"
  , "Commit       : \{commit}"
  , "ipkg File    : \{ipkg}"
  ]

  Local d i _ =>
    let ipkg := toAbsFile d i
     in [ "Type         : Local Idris project"
        , "Location     : \{ipkg.parent}"
        , "ipkg File    : \{ipkg.file}"
        ]

  Core _            => [
    "Type         : Idris core package"
  ]

namePlusModules : String -> ResolvedLib t -> String
namePlusModules n rl =
  unlines $  nameStr rl :: map (indent 2) (prettyModules n rl)

keep : QueryMode -> String -> ResolvedLib t -> Bool
keep PkgName    q p = isInfixOf q (nameStr p)
keep Dependency q p = any ((q ==) . value) (dependencies p)
keep Module     q p = any (isInfixOf q . show . fst) (modules p.desc.desc)

resultString :  Env s
             -> (query : String)
             -> QueryMode
             -> ResolvedLib t
             -> String
resultString e q Module rl = namePlusModules q rl
resultString e _ _      rl = case e.queryType of
  NameOnly => nameStr rl

  ShortDesc =>
    let Just d := shortDesc rl | Nothing => nameStr rl
     in "\{name rl}\n  \{d}\n"

  Dependencies =>
    let ds@(_ :: _) := deps rl | [] => nameStr rl
     in unlines $  nameStr rl :: map (indent 2 . interpolate) ds

  Details => unlines . (nameStr rl ::) . map (indent 2) $ concat [
      toList (("Brief        : " ++) <$> shortDesc rl)
    , details rl
    , prettyDeps rl
    ]

  Ipkg => unlines $ nameStr rl :: map (indent 2) (lines rl.desc.cont)

export
query :  HasIO io
      => QueryMode
      -> String
      -> (e : Env s)
      -> EitherT PackErr io ()
query m n e = do
  ss <- query_ e $ \p => toMaybe (keep m n p) (resultString e n m p)
  putStrLn $ unlines ss

--------------------------------------------------------------------------------
--          General Info
--------------------------------------------------------------------------------

public export
data InstallType = App | AppAndLib | Lib

public export
isApp : InstallType -> Bool
isApp App       = True
isApp AppAndLib = True
isApp Lib       = False

public export
isLib : InstallType -> Bool
isLib App       = False
isLib AppAndLib = True
isLib Lib       = True

public export
record PkgInfo where
  constructor MkPkgInfo
  name    : PkgName
  type    : InstallType
  details : Package

toInfo : HasIO io => Env e -> (PkgName,Package) -> io (Maybe PkgInfo)
toInfo e (n,p) = do
  isLib <- exists (pkgPathDir e n p)
  isApp <- exists (pkgBinDir e n p)
  pure $ case (isLib,isApp) of
    (True,True)   => Just $ MkPkgInfo n AppAndLib p
    (False,True)  => Just $ MkPkgInfo n App p
    (True,False)  => Just $ MkPkgInfo n Lib p
    (False,False) => Nothing

export
installed : HasIO io => Env s -> io (List PkgInfo)
installed e = mapMaybeM (toInfo e) (SortedMap.toList $ allPackages e)

apps : List PkgInfo -> String
apps ps = case filter (isApp . type) ps of
  []      => ""
  x :: xs => unlines $
    "\n\nInstalled Apps      : \{x.name}" ::
    map (indent 22 . value . name) xs

libs : List PkgInfo -> String
libs ps = case filter (isLib . type) ps of
  []      => ""
  x :: xs => unlines $
    "\nInstalled Libraries : \{x.name}" ::
    map (indent 22 . value . name) xs

export
infoString : Env s -> List PkgInfo -> String
infoString e ps = """
  Package Collection  : \{e.collection}
  Idris2 URL          : \{e.db.idrisURL}
  Idris2 Version      : \{e.db.idrisVersion}
  Idris2 Commit       : \{e.db.idrisCommit}
  Scheme Executable   : \{e.scheme}
  """ ++ apps ps ++ libs ps

export
printInfo : HasIO io => Env s -> io ()
printInfo e = do
  ps <- installed e
  putStrLn $ infoString e ps

--------------------------------------------------------------------------------
--          Fuzzy Search
--------------------------------------------------------------------------------

installedPkgs :  HasIO io
              => List PkgName
              -> Env HasIdris
              -> EitherT PackErr io (List $ ResolvedLib (), List $ ResolvedLib ())
installedPkgs ns e = do
  all <- mapMaybe id <$> traverse run (pkgNames e)
  pure (all, filter inPkgs all)
  where run : PkgName -> EitherT PackErr io (Maybe $ ResolvedLib ())
        run n = do
          rl <- resolveLib e n
          b  <- exists (pkgInstallDir e rl.name rl.pkg rl.desc)
          pure (toMaybe b rl)

        inPkgs : ResolvedLib t -> Bool
        inPkgs rl = isNil ns || elem (name rl) ns

imports : ResolvedLib t -> String
imports = unlines . map ("import " ++) . modules

pre : String
pre = "Main> "

noOut : String
noOut = "Main> \nMain> \n"

removePre : String -> String
removePre s = case isPrefixOf pre s of
  True  => pack . drop (length pre) $ unpack s
  False => s

fuzzyTrim : String -> String
fuzzyTrim = unlines
          . map removePre
          . filter (pre /=)
          . lines

fuzzyPkg :  HasIO io
         => String
         -> Env HasIdris
         -> (allPkgs   : List $ ResolvedLib t)
         -> ResolvedLib t
         -> EitherT PackErr io ()
fuzzyPkg q e allPkgs rl =
  let dir = tmpDir e
   in do
     mkDir dir
     finally (rmDir dir) $ inDir dir $ \d => do
       putStrLn "\{name rl}:\n"
       write (MkF d "test.idr") (imports rl)
       write (MkF d "input") ":fs \{q}\n"

       let (cmd,env) := idrisWithPkgs e allPkgs

       str <- sysRunWithEnv "\{cmd} --quiet --no-prelude --no-banner test.idr < input" env
       case noOut == str of
         True  => pure ()
         False => putStrLn (fuzzyTrim str)

export
fuzzy :  HasIO io
      => List PkgName
      -> String
      -> Env HasIdris
      -> EitherT PackErr io ()
fuzzy m q e = do
  (allPkgs,rps) <- installedPkgs m e
  traverse_ (fuzzyPkg q e allPkgs) rps
