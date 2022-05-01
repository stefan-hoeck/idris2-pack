module Pack.Database.Types

import Data.List1
import Data.String
import Idris.Package.Types
import Libraries.Data.SortedMap
import Libraries.Utils.Path
import Pack.Core.Types

%default total

--------------------------------------------------------------------------------
--          Packages
--------------------------------------------------------------------------------

||| Description of a GitHub or local Idris package in the
||| package database.
public export
data Package : Type where
  ||| A repository on GitHub, given as the package's name
  ||| (should be the same as in the `.ipkg` file), URL,
  ||| commit (hash or tag), and name of `.ipkg` file to use.
  GitHub :  (name   : PkgName)
         -> (url    : URL)
         -> (commit : Commit)
         -> (ipkg   : Path)
         -> Package

  ||| A local Idris project given as a name (same as in
  ||| `.ipkg` file), absolute path to package directory,
  ||| and `.ipkg` file to use.
  Local  :  (name   : PkgName)
         -> (dir    : Path)
         -> (ipkg   : Path)
         -> Package

namespace Package
  export
  name : Package -> PkgName
  name (GitHub n _ _ _) = n
  name (Local n _ _)    = n


||| A resolved package, which was downloaded from GitHub
||| or looked up in the local file system. This comes with
||| a fully parsed `PkgDesc` (representing the `.ipkg` file).
public export
data ResolvedPackage : Type where
  ||| A resolved GitHub project with parse `.ipkg` file.
  RGitHub :  (name   : PkgName)
          -> (url    : URL)
          -> (commit : Commit)
          -> (ipkg   : Path)
          -> (desc   : PkgDesc)
          -> ResolvedPackage

  ||| A local (and parsed) `.ipkg` file.
  RIpkg   :  (name : PkgName)
          -> (desc : PkgDesc)
          -> ResolvedPackage

  ||| A local project with (absolute) path to project's
  ||| directory and parsed `.ipkg` file.
  RLocal  :  (name : PkgName)
          -> (dir  : Path)
          -> (ipkg : Path)
          -> (desc : PkgDesc)
          -> ResolvedPackage

  ||| The *base* library from the Idris2 project.
  Base    : ResolvedPackage

  ||| The *contrib* library from the Idris2 project.
  Contrib : ResolvedPackage

  ||| The *idris2* (API) library from the Idris2 project.
  Idris2  : ResolvedPackage

  ||| The *linear* library from the Idris2 project.
  Linear  : ResolvedPackage

  ||| The *network* library from the Idris2 project.
  Network : ResolvedPackage

  ||| The *prelude* from the Idris2 project.
  Prelude : ResolvedPackage

  ||| The *test* library from the Idris2 project.
  Test    : ResolvedPackage

||| True, if the given resolved package represents
||| one of the core packages (`base`, `prelude`, etc.)
export
isCorePackage : ResolvedPackage -> Bool
isCorePackage (RGitHub _ _ _ _ _) = False
isCorePackage (RIpkg _ _)         = False
isCorePackage (RLocal _ _ _ _)    = False
isCorePackage Base                = True
isCorePackage Contrib             = True
isCorePackage Linear              = True
isCorePackage Idris2              = True
isCorePackage Network             = True
isCorePackage Prelude             = True
isCorePackage Test                = True

||| Try to extract the package description from a
||| resolved package.
export
desc : ResolvedPackage -> Maybe PkgDesc
desc (RGitHub _ _ _ _ d)  = Just d
desc (RIpkg _ d)          = Just d
desc (RLocal _ _ _ d)     = Just d
desc _                    = Nothing

||| Extracts the name of the executable (if any) from
||| a resolved package.
export
executable : ResolvedPackage -> Maybe String
executable p = desc p >>= executable

namespace ResolvedPackage
  ||| Extracts the package name from a resolved package.
  export
  name : ResolvedPackage -> PkgName
  name (RGitHub n _ _ _ _) = n
  name (RIpkg n _)         = n
  name (RLocal n _ _ _)    = n
  name Base                = "base"
  name Contrib             = "contrib"
  name Idris2              = "idris2"
  name Linear              = "linear"
  name Network             = "network"
  name Prelude             = "prelude"
  name Test                = "test"

--------------------------------------------------------------------------------
--          Package Database
--------------------------------------------------------------------------------

||| DB used for building packages. This includes
||| the Idris commit to use, together with a curated list of
||| known packages.
public export
record DB where
  constructor MkDB
  idrisCommit  : Commit
  idrisVersion : String
  packages     : SortedMap PkgName Package

--------------------------------------------------------------------------------
--          Reading a Database
--------------------------------------------------------------------------------

commaSep : String -> List String
commaSep = forget . split (',' ==)

||| Encode a package from the data collection as a comma
||| separated list.
export
printPkg : Package -> String
printPkg (Local n d p) = "\{n},\{show d},\{show p}"
printPkg (GitHub n u c p) = "\{n},\{u},\{c},\{show p}"

||| Tries to read a line in a package collection.
export
readPkg : String -> Either PackErr Package
readPkg s  = case commaSep s of
  [n,url,hash,pkg] =>
    Right $ GitHub (MkPkgName n) (MkURL url) (MkCommit hash) (parse pkg)
  [n,dir,pkg]      =>
    Right $ Local (MkPkgName n) (parse dir) (parse pkg)
  _                => Left (InvalidPackageDesc s)

export
readPkgs : List String -> Either PackErr (SortedMap PkgName Package)
readPkgs = map toPackageMap . traverse readPkg . filter (/= "")
  where toPackageMap : List Package -> SortedMap PkgName Package
        toPackageMap = SortedMap.fromList . map (\p => (name p,p))

export
readDB : String -> Either PackErr DB
readDB s = case lines s of
  []       => Left EmptyPkgDB
  (h :: t) => case commaSep h of
    [c,v] => MkDB (MkCommit c) v <$> readPkgs t
    _     => Left (InvalidDBHeader h)

export
printDB : DB -> String
printDB (MkDB c v ps) =
    unlines $ "\{c},\{v}" :: map printPkg (values ps)

-- --------------------------------------------------------------------------------
-- --          Report
-- --------------------------------------------------------------------------------
-- 
-- public export
-- data Report : Type where
--   Success : ResolvedPackage -> Report
--   Failure : ResolvedPackage -> List String -> Report
--   Error   : String -> PackErr -> Report
-- 
-- public export
-- 0 ReportDB : Type
-- ReportDB = SortedMap String Report
-- 
-- export
-- failingDependencies : List Report -> List String
-- failingDependencies rs = nub $ rs >>=
--   \case Success _    => []
--         Failure _ ss => ss
--         Error s err  => [s]
-- 
-- record RepLines where
--   constructor MkRL
--   errs      : SnocList String
--   failures  : SnocList String
--   successes : SnocList String
-- 
-- Semigroup RepLines where
--   MkRL e1 f1 s1 <+> MkRL e2 f2 s2 = MkRL (e1 <+> e2) (f1 <+> f2) (s1 <+> s2)
-- 
-- Monoid RepLines where
--   neutral = MkRL Lin Lin Lin
-- 
-- report : RepLines -> String
-- report (MkRL errs fails succs) = """
--   Packages failing to resolve:
-- 
--   \{unlines $ errs <>> Nil}
-- 
--   Packages failing to build:
-- 
--   \{unlines $ fails <>> Nil}
-- 
--   Packages building successfully:
-- 
--   \{unlines $ succs <>> Nil}
--   """
-- 
-- toRepLines : Report -> RepLines
-- toRepLines (Success x) =
--   MkRL Lin Lin (Lin :< "  \{name x}")
-- 
-- toRepLines (Failure x []) =
--   MkRL Lin (Lin :< "  \{name x}") Lin
-- 
-- toRepLines (Failure x ds) =
--   let fl   = "  \{name x}"
--       deps = concat $ intersperse ", " ds
--       sl   = "  failing dependencies: \{deps}"
--    in MkRL Lin [< fl,sl] Lin
-- toRepLines (Error x y) =
--   MkRL [< "  \{x}: \{printErr y}"] Lin Lin
-- 
-- 
-- export
-- printReport : ReportDB -> String
-- printReport = report . foldMap toRepLines
