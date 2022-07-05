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
import Pack.Version

%default total

--------------------------------------------------------------------------------
--         QPkg
--------------------------------------------------------------------------------

public export
record AppInfo (p : Package) where
  constructor AI
  exec   : Body
  status : PkgStatus p

public export
record QPkg where
  constructor QP
  lib : ResolvedLib U
  app : Maybe (AppInfo lib.pkg)

export %inline
name : QPkg -> PkgName
name = name . lib

export %inline
nameStr : QPkg -> String
nameStr = value . name

export %inline
dependencies : QPkg -> List PkgName
dependencies = dependencies . lib

export
isApp : QPkg -> Bool
isApp (QP _ a) = isJust a

export
installedLib : QPkg -> Bool
installedLib qp = case qp.lib.status of
  Installed => True
  Outdated  => True
  Missing   => False

export
installedApp : QPkg -> Bool
installedApp qp = case map status qp.app of
  Just Installed => True
  Just Outdated  => True
  Just Missing   => False
  Nothing        => False

resolve : HasIO io => Env s -> PkgName -> EitherT PackErr io QPkg
resolve e n = do
  lib <- resolveLib e n
  Just exe <- pure (exec lib.desc) | Nothing => pure (QP lib Nothing)
  st       <- appStatus e n lib.pkg lib.desc exe
  pure $ QP lib (Just $ AI exe st)

pkgNames : Env s -> List PkgName
pkgNames e = keys (allPackages e)

export
resolveAll : HasIO io => Env s -> EitherT PackErr io (List QPkg)
resolveAll e = traverse (resolve e) $ pkgNames e

--------------------------------------------------------------------------------
--         Queries
--------------------------------------------------------------------------------

query_ :  HasIO io
       => (e : Env s)
       -> (q : QPkg -> Maybe b)
       -> EitherT PackErr io (List b)
query_ e q = mapMaybe q <$> resolveAll e

shortDesc : QPkg -> Maybe String
shortDesc q = q.lib.desc.desc.brief

modules : QPkg -> List String
modules = map (show . fst) . modules . desc . desc . lib

prettyDeps : QPkg -> List String
prettyDeps qp = case dependencies qp of
  []     => ["Dependencies :"]
  h :: t => "Dependencies : \{h}" :: map (indent 15 . interpolate) t

prettyModules : String -> QPkg -> List String
prettyModules s qp = case filter (isInfixOf s) (modules qp) of
  []     => ["Modules :"]
  h :: t => "Modules : \{h}" :: map (indent 10) t

status : PkgStatus p -> String
status Missing   = "not installed"
status Installed = "installed"
status Outdated  = "outdated"

libStatus : QPkg -> List String
libStatus q = [ "Library      : \{status q.lib.status}" ]

appStatus : QPkg -> List String
appStatus qp = case qp.app of
  Nothing          => []
  Just (AI exe st) =>
    [ "Executable   : \{exe}"
    , "App          : \{status st}"
    ]

details : QPkg -> List String
details qp = case qp.lib.pkg of
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

namePlusModules : String -> QPkg -> String
namePlusModules n qp =
  unlines $  nameStr qp :: map (indent 2) (prettyModules n qp)

keep : QueryMode -> String -> QPkg -> Bool
keep PkgName    q p = isInfixOf q (nameStr p)
keep Dependency q p = any ((q ==) . value) (dependencies p)
keep Module     q p = any (isInfixOf q . show . fst) (modules p.lib.desc.desc)

resultString :  Env s -> (query : String) -> QueryMode -> QPkg -> String
resultString e q Module qp = namePlusModules q qp
resultString e _ _      qp = case e.queryType of
  NameOnly => nameStr qp

  ShortDesc =>
    let Just d := shortDesc qp | Nothing => nameStr qp
     in "\{name qp}\n  \{d}\n"

  Dependencies =>
    let ds@(_ :: _) := dependencies qp | [] => nameStr qp
     in unlines $  nameStr qp :: map (indent 2 . interpolate) ds

  Details => unlines . (nameStr qp ::) . map (indent 2) $ concat [
      toList (("Brief        : " ++) <$> shortDesc qp)
    , details qp
    , libStatus qp
    , appStatus qp
    , prettyDeps qp
    ]

  Ipkg => unlines $ nameStr qp :: map (indent 2) (lines qp.lib.desc.cont)

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

instLib : QPkg -> Maybe String
instLib qp = case qp.lib.status of
  Installed => Just "\{qp.lib.name}"
  Outdated  => Just "\{qp.lib.name} (outdated)"
  Missing   => Nothing

instApp : QPkg -> Maybe String
instApp (QP lib $ Just (AI _ st))  = case st of
  Installed => Just "\{lib.name}"
  Outdated  => Just "\{lib.name} (outdated)"
  Missing   => Nothing
instApp _ = Nothing

apps : List QPkg -> String
apps ps = case mapMaybe instApp ps of
  []      => ""
  x :: xs => unlines $
    "\n\nInstalled Apps      : \{x}" ::
    map (indent 22) xs

libs : List QPkg -> String
libs ps = case mapMaybe instLib ps of
  []      => ""
  x :: xs => unlines $
    "\nInstalled Libraries : \{x}" ::
    map (indent 22) xs

export
infoString : Env s -> List QPkg -> String
infoString e ps = """
  Package Collection  : \{e.collection}
  Idris2 URL          : \{e.db.idrisURL}
  Idris2 Version      : \{e.db.idrisVersion}
  Idris2 Commit       : \{e.db.idrisCommit}
  Scheme Executable   : \{e.scheme}
  Pack Commit         : \{Version.version}
  """ ++ apps ps ++ libs ps

export
printInfo : HasIO io => Env s -> EitherT PackErr io ()
printInfo e = resolveAll e >>= putStrLn . infoString e

--------------------------------------------------------------------------------
--          Fuzzy Search
--------------------------------------------------------------------------------

installedPkgs :  HasIO io
              => List PkgName
              -> Env HasIdris
              -> EitherT PackErr io (List QPkg, List QPkg)
installedPkgs ns e = do
  all <- filter installedLib <$> resolveAll e
  pure (all, filter inPkgs all)
  where inPkgs : QPkg -> Bool
        inPkgs qp = isNil ns || elem (name qp) ns

imports : QPkg -> String
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
         -> (allPkgs   : List QPkg)
         -> QPkg
         -> EitherT PackErr io ()
fuzzyPkg q e allPkgs qp =
  let dir = tmpDir e
   in do
     mkDir dir
     finally (rmDir dir) $ inDir dir $ \d => do
       putStrLn "\{name qp}:\n"
       write (MkF d "test.idr") (imports qp)
       write (MkF d "input") ":fs \{q}\n"

       let (cmd,env) := idrisWithPkgs e (map lib allPkgs)

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
