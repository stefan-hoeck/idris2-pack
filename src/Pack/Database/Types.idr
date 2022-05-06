module Pack.Database.Types

import Data.List1
import Data.SortedMap
import Data.String
import Idris.Package.Types
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
  RIpkg   :  (path : Path)
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

||| Extracts the dependencies of a resolved package.
export
dependencies : ResolvedPackage -> List PkgRep
dependencies rp = case desc rp of
  Just d  => map (Pkg . MkPkgName . pkgname) d.depends
  Nothing => []

||| Extracts the names of dependencies of a resolved package.
export
depNames : ResolvedPackage -> List PkgName
depNames rp = case desc rp of
  Just d  => map (MkPkgName . pkgname) d.depends
  Nothing => []

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
  name (RIpkg _ d)         = MkPkgName d.name
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
  idrisVersion : PkgVersion
  packages     : SortedMap PkgName Package
