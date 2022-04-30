module Pack.Types

import Data.List1
import Data.String
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

  ||| Link to the github page
  url    : String

  ||| Version (git commit) to use
  commit : String

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
  RP :  (pkg  : Package)
     -> (desc : PkgDesc)
     -> ResolvedPackage

  Local :  (name : String)
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
isCorePackage (RP _ _)    = False
isCorePackage (Local _ _) = False
isCorePackage Base        = True
isCorePackage Contrib     = True
isCorePackage Linear      = True
isCorePackage Idris2      = True
isCorePackage Network     = True
isCorePackage Prelude     = True
isCorePackage Test        = True

export
desc : ResolvedPackage -> Maybe PkgDesc
desc (RP _ d)    = Just d
desc (Local _ d) = Just d
desc _           = Nothing

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
  packages     : List Package

--------------------------------------------------------------------------------
--          Build Environment
--------------------------------------------------------------------------------

||| Environment used for building packages.
public export
record Env where
  constructor MkEnv
  db   : DB
  conf : Config

export %hint
envToConfig : Env => Config
envToConfig = conf %search

--------------------------------------------------------------------------------
--          Reading a Database
--------------------------------------------------------------------------------

commaSep : String -> List String
commaSep = forget . split (',' ==)

readPkg : String -> Either PackErr Package
readPkg s = case commaSep s of
  [n,url,hash,pkg] => Right $ MkPackage n url hash pkg
  _                => Left (InvalidPackageDesc s)

export
readDB : String -> Either PackErr DB
readDB s = case lines s of
  []       => Left EmptyPkgDB
  (h :: t) => case commaSep h of
    [c,v] => MkDB c v <$> traverse readPkg t
    _     => Left (InvalidDBHeader h)
