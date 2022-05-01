module Pack.Types

import Data.List
import Data.List1
import Data.SnocList
import Data.String
import Libraries.Data.SortedMap
import Pack.CmdLn
import Pack.Err
import Idris.Package.Types

%default total

--------------------------------------------------------------------------------
--          Packages
--------------------------------------------------------------------------------

||| Description of an Idris package on GitHub
|||
||| Eventually, I'd like to list these in a data base
||| (probably just a clear-text file) of curated Idris
||| packages together with an Idris version to use for them.
public export
record Package where
  constructor MkPackage
  ||| Name of the package. This should be the
  ||| same as the one given in the .ipkg file
  ||| given below.
  name   : String

  ||| Link to the github page or absolute path to
  ||| local project
  url    : String

  ||| Version (git commit) to use (Nothing in case
  ||| of a local project)
  commit : Maybe String

  ||| Name of ".ipkg" file
  ipkg   : String

||| A resolved package, which was downloaded from GitHub
||| and the version and dependencies of which where extracted
||| from its ".ipkg" file.
|||
||| Eventually, I'd like to generate a DB (probably just a
||| text file) of resolved packages from a list of packages.
public export
data ResolvedPackage : Type where
  RP :  (name   : String)
     -> (url    : String)
     -> (commit : String)
     -> (ipkg   : String)
     -> (desc   : PkgDesc)
     -> ResolvedPackage

  Ipkg :  (name : String)
       -> (desc : PkgDesc)
       -> ResolvedPackage

  Local :  (name : String)
        -> (dir  : String)
        -> (ipkg : String)
        -> (desc : PkgDesc)
        -> ResolvedPackage

  Base    : ResolvedPackage
  Contrib : ResolvedPackage
  Idris2  : ResolvedPackage
  Linear  : ResolvedPackage
  Network : ResolvedPackage
  Prelude : ResolvedPackage
  Test    : ResolvedPackage

export
isCorePackage : ResolvedPackage -> Bool
isCorePackage (RP _ _ _ _ _)  = False
isCorePackage (Ipkg _ _)      = False
isCorePackage (Local _ _ _ _) = False
isCorePackage Base            = True
isCorePackage Contrib         = True
isCorePackage Linear          = True
isCorePackage Idris2          = True
isCorePackage Network         = True
isCorePackage Prelude         = True
isCorePackage Test            = True

export
desc : ResolvedPackage -> Maybe PkgDesc
desc (RP _ _ _ _ d)  = Just d
desc (Ipkg _ d)      = Just d
desc (Local _ _ _ d) = Just d
desc _               = Nothing

export
executable : ResolvedPackage -> Maybe String
executable p = desc p >>= executable

export
name : ResolvedPackage -> String
name (RP n _ _ _ _)  = n
name (Ipkg n _)      = n
name (Local n _ _ _) = n
name Base            = "base"
name Contrib         = "contrib"
name Idris2          = "idris2"
name Linear          = "linear"
name Network         = "network"
name Prelude         = "prelude"
name Test            = "test"

--------------------------------------------------------------------------------
--          Package Database
--------------------------------------------------------------------------------

||| DB used for building packages. This includes
||| the Idris commit to use, together with a curated list of
||| known packages.
public export
record DB where
  constructor MkDB
  idrisCommit  : String
  idrisVersion : String
  packages     : SortedMap String Package

--------------------------------------------------------------------------------
--          Build Environment
--------------------------------------------------------------------------------

||| Environment used for building packages.
public export
record Env where
  constructor MkEnv
  db   : DB
  conf : Config

--------------------------------------------------------------------------------
--          Reading a Database
--------------------------------------------------------------------------------

commaSep : String -> List String
commaSep = forget . split (',' ==)

export
printPkg : Package -> String
printPkg (MkPackage n u Nothing p) = "\{n},\{u},\{p}"
printPkg (MkPackage n u (Just c) p) = "\{n},\{u},\{c},\{p}"

export
readPkg : String -> Either PackErr Package
readPkg s  = case commaSep s of
  [n,url,hash,pkg] => Right $ MkPackage n url (Just hash) pkg
  [n,url,pkg]      => Right $ MkPackage n url Nothing pkg
  _                => Left (InvalidPackageDesc s)

export
readPkgs : List String -> Either PackErr (SortedMap String Package)
readPkgs = map toPackageMap . traverse readPkg . filter (/= "")
  where toPackageMap : List Package -> SortedMap String Package
        toPackageMap = fromList . map (\p => (p.name,p))


export
readDB : String -> Either PackErr DB
readDB s = case lines s of
  []       => Left EmptyPkgDB
  (h :: t) => case commaSep h of
    [c,v] => MkDB c v <$> readPkgs t
    _     => Left (InvalidDBHeader h)

export
printDB : DB -> String
printDB (MkDB c v ps) =
    unlines $ "\{c},\{v}" :: map printPkg (values ps)

--------------------------------------------------------------------------------
--          Report
--------------------------------------------------------------------------------

public export
data Report : Type where
  Success : ResolvedPackage -> Report
  Failure : ResolvedPackage -> List String -> Report
  Error   : String -> PackErr -> Report

public export
0 ReportDB : Type
ReportDB = SortedMap String Report

export
failingDependencies : List Report -> List String
failingDependencies rs = nub $ rs >>=
  \case Success _    => []
        Failure _ ss => ss
        Error s err  => [s]

record RepLines where
  constructor MkRL
  errs      : SnocList String
  failures  : SnocList String
  successes : SnocList String

Semigroup RepLines where
  MkRL e1 f1 s1 <+> MkRL e2 f2 s2 = MkRL (e1 <+> e2) (f1 <+> f2) (s1 <+> s2)

Monoid RepLines where
  neutral = MkRL Lin Lin Lin

report : RepLines -> String
report (MkRL errs fails succs) = """
  Packages failing to resolve:

  \{unlines $ errs <>> Nil}

  Packages failing to build:

  \{unlines $ fails <>> Nil}

  Packages building successfully:

  \{unlines $ succs <>> Nil}
  """

toRepLines : Report -> RepLines
toRepLines (Success x) =
  MkRL Lin Lin (Lin :< "  \{name x}")

toRepLines (Failure x []) =
  MkRL Lin (Lin :< "  \{name x}") Lin

toRepLines (Failure x ds) =
  let fl   = "  \{name x}"
      deps = concat $ intersperse ", " ds
      sl   = "  failing dependencies: \{deps}"
   in MkRL Lin [< fl,sl] Lin
toRepLines (Error x y) =
  MkRL [< "  \{x}: \{printErr y}"] Lin Lin


export
printReport : ReportDB -> String
printReport = report . foldMap toRepLines
