module Pack.Runner.Query

import Core.Name.Namespace
import Data.List
import Data.Maybe
import Data.SortedMap
import Data.String
import Idris.Package.Types
import Pack.CmdLn.Types
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types
import Pack.Runner.Database
import Pack.Runner.Install

%default total

pkgNames : Env s -> List PkgName
pkgNames e = sort
           $ [ "prelude"
             , "base"
             , "contrib"
             , "idris2"
             , "network"
             , "test"
             , "linear"
             ] ++ keys (allPackages e)

query_ :  HasIO io
       => (e : Env s)
       -> (q : String -> ResolvedPackage -> Maybe b)
       -> EitherT PackErr io (List b)
query_ e q = mapMaybe id <$> traverse run (pkgNames e)
  where run : PkgName -> EitherT PackErr io (Maybe b)
        run n = (\(s,rp) => q s rp) <$> resolvePair e (Pkg n)

shortDesc : ResolvedPackage -> Maybe String
shortDesc = brief . desc

deps : ResolvedPackage -> List String
deps = map pkgname . depends . desc

modules : ResolvedPackage -> List String
modules = map (show . fst) . modules . desc

prettyDeps : ResolvedPackage -> List String
prettyDeps rp = case deps rp of
  []     => ["Dependencies :"]
  h :: t => "Dependencies : \{h}" :: map (indent 15) t

prettyModules : String -> ResolvedPackage -> List String
prettyModules s rp = case filter (isInfixOf s) (modules rp) of
  []     => ["Modules :"]
  h :: t => "Modules : \{h}" :: map (indent 10) t

details : ResolvedPackage -> List String
details (RGitHub name url commit ipkg _ desc) = [
    "Type         : GitHub project"
  , "URL          : \{url}"
  , "Commit       : \{commit}"
  , "ipkg File    : \{ipkg}"
  ]

details (RIpkg path desc) = [
    "Type         : Local ipkg file"
  , "Path         : \{path}"
  ]

details (RLocal name dir ipkg _ desc) = [
    "Type         : Local Idris project"
  , "Location     : \{dir}"
  , "ipkg File    : \{ipkg}"
  ]

details (Core _ _) = [ "Type         : Idris core package" ]

namePlusModules : String -> ResolvedPackage -> String
namePlusModules n rp =
  unlines $  nameStr rp :: map (indent 2) (prettyModules n rp)

keep : QueryMode -> String -> ResolvedPackage -> Bool
keep PkgName    q p = isInfixOf q (nameStr p)
keep Dependency q p = any ((q ==) . pkgname) (depends $ desc p)
keep Module     q p = any (isInfixOf q . show . fst) (modules $ desc p)

resultString :  Env s
             -> (query : String)
             -> QueryMode
             -> (ipkg  : String)
             -> ResolvedPackage
             -> String
resultString e q Module _    rp = namePlusModules q rp
resultString e _ _      ipkg rp = case e.queryType of
  NameOnly => nameStr rp

  ShortDesc =>
    let Just d := shortDesc rp | Nothing => nameStr rp
     in "\{name rp}\n  \{d}\n"

  Dependencies =>
    let ds@(_ :: _) := deps rp | [] => nameStr rp
     in unlines $  nameStr rp :: map (indent 2) ds

  Details => unlines . (nameStr rp ::) . map (indent 2) $ concat [
      toList (("Brief        : " ++) <$> shortDesc rp)
    , details rp
    , prettyDeps rp
    ]

  Ipkg => unlines $ nameStr rp :: map (indent 2) (lines ipkg)

export
query :  HasIO io
      => QueryMode
      -> String
      -> (e : Env s)
      -> EitherT PackErr io ()
query m n e = do
  ss <- query_ e $ \s,p => toMaybe (keep m n p) (resultString e n m s p)
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
toInfo e p = do
  isLib <- exists (pkgPathDir e p)
  isApp <- exists (pkgBinDir e p)
  pure $ case (isLib,isApp) of
    (True,True)   => Just $ MkPkgInfo (fst p) AppAndLib (snd p)
    (False,True)  => Just $ MkPkgInfo (fst p) App (snd p)
    (True,False)  => Just $ MkPkgInfo (fst p) Lib (snd p)
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
              -> EitherT PackErr io (List ResolvedPackage, List ResolvedPackage)
installedPkgs ns e = do
  all <- mapMaybe id <$> traverse run (pkgNames e)
  pure (all, filter inPkgs all)
  where run : PkgName -> EitherT PackErr io (Maybe ResolvedPackage)
        run n = do
          rp <- resolve e (Pkg n)
          b  <- exists (packageInstallDir e rp)
          pure (toMaybe b rp)

        inPkgs : ResolvedPackage -> Bool
        inPkgs rp = isNil ns || elem (name rp) ns

imports : ResolvedPackage -> String
imports = unlines . map ("import " ++) . modules

noOut : String
noOut = "Main> \nMain> \n"

fuzzyPkg :  HasIO io
         => String
         -> Env HasIdris
         -> (allPkgs   : List ResolvedPackage)
         -> ResolvedPackage
         -> EitherT PackErr io ()
fuzzyPkg q e allPkgs rp =
  let dir = tmpDir e
   in do
     mkDir dir
     finally (rmDir dir) $ inDir dir $ do
       putStrLn "\{name rp}:\n"
       write (parse "test.idr") (imports rp)
       write (parse "input") ":fs \{q}\n"
       str <- sysRun "\{idrisWithPkgs e allPkgs} --quiet --no-prelude --no-banner test.idr < input"
       case noOut == str of
         True  => pure ()
         False => putStrLn str

export
fuzzy :  HasIO io
      => List PkgName
      -> String
      -> Env HasIdris
      -> EitherT PackErr io ()
fuzzy m q e = do
  (allPkgs,rps) <- installedPkgs m e
  traverse_ (fuzzyPkg q e allPkgs) rps
