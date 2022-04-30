module Pack.Types

import Data.List1
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
