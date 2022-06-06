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
|||
||| Note: This does not contain the package name, as it
||| will be paired with its name in a `SortedMap`.
public export
data Package : Type where
  ||| A repository on GitHub, given as the package's URL,
  ||| commit (hash or tag), and name of `.ipkg` file to use.
  ||| `pkgPath` should be set to `True` for executable which need
  ||| access to the `IDRIS2_PACKAGE_PATH`: The list of directories
  ||| where Idris packages are installed.
  GitHub :  (url     : URL)
         -> (commit  : Commit)
         -> (ipkg    : Path)
         -> (pkgPath : Bool)
         -> Package

  ||| A local Idris project given as an absolute path to a local
  ||| directory, and `.ipkg` file to use.
  ||| `pkgPath` should be set to `True` for executable which need
  ||| access to the `IDRIS2_PACKAGE_PATH`: The list of directories
  ||| where Idris packages are installed.
  Local  :  (dir     : Path)
         -> (ipkg    : Path)
         -> (pkgPath : Bool)
         -> Package

||| A resolved package, which was downloaded from GitHub
||| or looked up in the local file system. This comes with
||| a fully parsed `PkgDesc` (representing the `.ipkg` file).
public export
data ResolvedPackage : Type where
  ||| A resolved GitHub project with parse `.ipkg` file.
  ||| `pkgPath` should be set to `True` for executable which need
  ||| access to the `IDRIS2_PACKAGE_PATH`: The list of directories
  ||| where Idris packages are installed.
  RGitHub :  (name    : PkgName)
          -> (url     : URL)
          -> (commit  : Commit)
          -> (ipkg    : Path)
          -> (pkgPath : Bool)
          -> (desc    : PkgDesc)
          -> ResolvedPackage

  ||| A local (and parsed) `.ipkg` file.
  RIpkg   :  (path : Path)
          -> (desc : PkgDesc)
          -> ResolvedPackage

  ||| A local project with (absolute) path to project's
  ||| directory and parsed `.ipkg` file.
  ||| `pkgPath` should be set to `True` for executable which need
  ||| access to the `IDRIS2_PACKAGE_PATH`: The list of directories
  ||| where Idris packages are installed.
  RLocal  :  (name    : PkgName)
          -> (dir     : Path)
          -> (ipkg    : Path)
          -> (pkgPath : Bool)
          -> (desc    : PkgDesc)
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
isCorePackage (RGitHub _ _ _ _ _ _) = False
isCorePackage (RIpkg _ _)           = False
isCorePackage (RLocal _ _ _ _ _)    = False
isCorePackage Base                  = True
isCorePackage Contrib               = True
isCorePackage Linear                = True
isCorePackage Idris2                = True
isCorePackage Network               = True
isCorePackage Prelude               = True
isCorePackage Test                  = True

||| Try to extract the package description from a
||| resolved package.
export
desc : ResolvedPackage -> Maybe PkgDesc
desc (RGitHub _ _ _ _ _ d)  = Just d
desc (RIpkg _ d)            = Just d
desc (RLocal _ _ _ _ d)     = Just d
desc _                      = Nothing

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

||| Extracts the package name from a resolved package.
export
name : ResolvedPackage -> PkgName
name (RGitHub n _ _ _ _ _) = n
name (RIpkg _ d)           = MkPkgName d.name
name (RLocal n _ _ _ _)    = n
name Base                  = "base"
name Contrib               = "contrib"
name Idris2                = "idris2"
name Linear                = "linear"
name Network               = "network"
name Prelude               = "prelude"
name Test                  = "test"

--------------------------------------------------------------------------------
--          Package Database
--------------------------------------------------------------------------------

||| DB used for building packages. This includes
||| the Idris commit to use, together with a curated list of
||| known packages.
public export
record DB where
  constructor MkDB
  idrisURL     : URL
  idrisCommit  : Commit
  idrisVersion : PkgVersion
  packages     : SortedMap PkgName Package

tomlBool : Bool -> String
tomlBool True  = "true"
tomlBool False = "false"

printPair : (PkgName,Package) -> String
printPair (x, GitHub url commit ipkg pp) =
  """

  [db.\{x}]
  type        = "github"
  url         = "\{url}"
  commit      = "\{commit}"
  ipkg        = "\{ipkg}"
  packagePath = "\{tomlBool pp}"
  """

printPair (x, Local dir ipkg pp) =
  """

  [db.\{x}]
  type        = "local"
  path        = "\{dir}"
  ipkg        = "\{ipkg}"
  packagePath = "\{tomlBool pp}"
  """

export
printDB : DB -> String
printDB (MkDB u c v db) =
  let header = """
        [idris2]
        url     = "\{u}"
        version = "\{v}"
        commit  = "\{c}"
        """
   in unlines $ header :: map printPair (SortedMap.toList db)
