module Pack.Runner.Query

import Core.Name.Namespace
import Data.List
import Data.IORef
import Data.Maybe
import Data.SortedMap
import Idris.Package.Types
import Pack.CmdLn.Types
import Pack.Config
import Pack.Core
import Pack.Database
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
  status : AppStatus p

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
  Installed _ => True
  Outdated    => True
  Missing     => False

export
installedApp : QPkg -> Bool
installedApp qp = case map status qp.app of
  Just Installed    => True
  Just BinInstalled => True
  Just Outdated     => True
  Just Missing      => False
  Nothing           => False

covering
resolve : HasIO io => Env => PkgName -> EitherT PackErr io QPkg
resolve n = do
  lib <- resolveLib n
  Just exe <- pure (exec lib.desc) | Nothing => pure (QP lib Nothing)
  st       <- appStatus n lib.pkg lib.desc lib.deps exe
  pure $ QP lib (Just $ AI exe st)

pkgNames : Env => List PkgName
pkgNames = keys allPackages

export covering
resolveAll : HasIO io => Env => EitherT PackErr io (List QPkg)
resolveAll = traverse resolve pkgNames

--------------------------------------------------------------------------------
--         Queries
--------------------------------------------------------------------------------

covering
query_ :  HasIO io
       => Env
       => (q : QPkg -> Maybe b)
       -> EitherT PackErr io (List b)
query_ q = mapMaybe q <$> resolveAll

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
status Missing        = "not installed"
status (Installed _ ) = "installed"
status Outdated       = "outdated"

status' : AppStatus p -> String
status' Missing      = "not installed"
status' Installed    = "installed"
status' BinInstalled = "installed and on $PATH"
status' Outdated     = "outdated"

libStatus : QPkg -> List String
libStatus q = [ "Library      : \{status q.lib.status}" ]

appStatus : QPkg -> List String
appStatus qp = case qp.app of
  Nothing          => []
  Just (AI exe st) =>
    [ "Executable   : \{exe}"
    , "App          : \{status' st}"
    ]

testFile : Maybe (File Rel) -> List String
testFile Nothing  = []
testFile (Just f) = ["Test File    : \{f}"]

details : QPkg -> List String
details qp = case qp.lib.pkg of
  GitHub url commit ipkg _ t => [
    "Type         : GitHub project"
  , "URL          : \{url}"
  , "Commit       : \{commit}"
  , "ipkg File    : \{ipkg}"
  ] ++ testFile t

  Local d i _ t =>
    let ipkg := toAbsFile d i
     in [ "Type         : Local Idris project"
        , "Location     : \{ipkg.parent}"
        , "ipkg File    : \{ipkg.file}"
        ] ++ testFile t

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

resultString : (c : Config) => (query : String) -> QueryMode -> QPkg -> String
resultString q Module qp = namePlusModules q qp
resultString _ _      qp = case c.queryType of
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

export covering
query :  HasIO io
      => QueryMode
      -> String
      -> Env
      -> EitherT PackErr io ()
query m n e = do
  ss <- query_ $ \p => toMaybe (keep m n p) (resultString n m p)
  putStrLn $ unlines ss

--------------------------------------------------------------------------------
--          General Info
--------------------------------------------------------------------------------

instLib : QPkg -> Maybe String
instLib qp = case qp.lib.status of
  Installed _ => Just "\{qp.lib.name}"
  Outdated    => Just "\{qp.lib.name} (outdated)"
  Missing     => Nothing

instApp : QPkg -> Maybe String
instApp (QP lib $ Just (AI _ st))  = case st of
  Installed    => Just "\{lib.name} (not on $PATH)"
  BinInstalled => Just "\{lib.name}"
  Outdated     => Just "\{lib.name} (outdated)"
  Missing      => Nothing
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
infoString : (e : Env) => List QPkg -> String
infoString ps = """
  Package Collection  : \{e.config.collection}
  Idris2 URL          : \{e.db.idrisURL}
  Idris2 Version      : \{e.db.idrisVersion}
  Idris2 Commit       : \{e.db.idrisCommit}
  Scheme Executable   : \{e.config.scheme}
  Pack Commit         : \{Version.version}
  """ ++ apps ps ++ libs ps

export covering
printInfo : HasIO io => Env -> EitherT PackErr io ()
printInfo e = resolveAll >>= putStrLn . infoString

--------------------------------------------------------------------------------
--          Fuzzy Search
--------------------------------------------------------------------------------

covering
installedPkgs :  HasIO io
              => IdrisEnv
              => List PkgName
              -> EitherT PackErr io (List QPkg, List QPkg)
installedPkgs ns = do
  all <- filter installedLib <$> resolveAll
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
         => IdrisEnv
         => String
         -> (allPkgs   : List QPkg)
         -> QPkg
         -> EitherT PackErr io ()
fuzzyPkg q allPkgs qp = do
  mkDir tmpDir
  finally (rmDir tmpDir) $ inDir tmpDir $ \d => do
    putStrLn "\{name qp}:\n"
    write (MkF d "test.idr") (imports qp)
    write (MkF d "input") ":fs \{q}\n"

    (cmd,env) <- idrisWithPkgs (map lib allPkgs)

    str <- sysRunWithEnv (cmd ++ ["--quiet", "--no-prelude", "--no-banner", "test.idr", NoEscape "<", "input"]) env
    case noOut == str of
      True  => pure ()
      False => putStrLn (fuzzyTrim str)

export covering
fuzzy :  HasIO io
      => List PkgName
      -> String
      -> IdrisEnv
      -> EitherT PackErr io ()
fuzzy m q e = do
  (allPkgs,rps) <- installedPkgs m
  traverse_ (fuzzyPkg q allPkgs) rps
