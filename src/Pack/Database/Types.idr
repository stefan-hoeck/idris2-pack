module Pack.Database.Types

import Data.List1
import Data.SortedMap
import Data.String
import Idris.Package.Types
import Pack.Core.Types

%default total

--------------------------------------------------------------------------------
--          MetaCommits
--------------------------------------------------------------------------------

||| A git commit hash or tag, or a meta commit: The latest commit of a branch.
public export
data MetaCommit : Type where
  MC     : Commit -> MetaCommit
  Latest : String -> MetaCommit

public export
FromString MetaCommit where
  fromString s = case forget $ split (':' ==) s of
    ["latest",branch] => Latest branch
    _                 => MC $ MkCommit s

export
Interpolation MetaCommit where
  interpolate (Latest b) = "latest:\{b}"
  interpolate (MC c)     = "\{c}"

--------------------------------------------------------------------------------
--          Packages
--------------------------------------------------------------------------------

||| Description of a GitHub or local Idris package in the
||| package database.
|||
||| Note: This does not contain the package name, as it
||| will be paired with its name in a `SortedMap`.
public export
data Package_ : (c : Type) -> Type where
  ||| A repository on GitHub, given as the package's URL,
  ||| commit (hash or tag), and name of `.ipkg` file to use.
  ||| `pkgPath` should be set to `True` for executable which need
  ||| access to the `IDRIS2_PACKAGE_PATH`: The list of directories
  ||| where Idris packages are installed.
  GitHub :  (url     : URL)
         -> (commit  : c)
         -> (ipkg    : Path Rel)
         -> (pkgPath : Bool)
         -> Package_ c

  ||| A local Idris project given as an absolute path to a local
  ||| directory, and `.ipkg` file to use.
  ||| `pkgPath` should be set to `True` for executable which need
  ||| access to the `IDRIS2_PACKAGE_PATH`: The list of directories
  ||| where Idris packages are installed.
  Local  :  (dir     : Path Abs)
         -> (ipkg    : Path Rel)
         -> (pkgPath : Bool)
         -> Package_ c

public export
0 Package : Type
Package = Package_ Commit

public export
0 UserPackage : Type
UserPackage = Package_ MetaCommit

||| Core packages bundled with the Idris compiler
public export
data CorePkg =
    Prelude
  | Base
  | Contrib
  | Linear
  | Network
  | Test
  | IdrisApi

export
Interpolation CorePkg where
  interpolate Prelude  = "prelude"
  interpolate Base     = "base"
  interpolate Contrib  = "contrib"
  interpolate Linear   = "linear"
  interpolate Network  = "network"
  interpolate Test     = "test"
  interpolate IdrisApi = "idris2"

export
ToBody CorePkg where
  toBody Prelude  = "prelude"
  toBody Base     = "base"
  toBody Contrib  = "contrib"
  toBody Linear   = "linear"
  toBody Network  = "network"
  toBody Test     = "test"
  toBody IdrisApi = "idris2"

export %inline
ToRelPath CorePkg where
  relPath c = PRel [< toBody c]

export
coreIpkgFile : CorePkg -> Body
coreIpkgFile IdrisApi = "idris2api.ipkg"
coreIpkgFile c        = toBody c <+> ".ipkg"

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
          -> (ipkg    : Path Rel)
          -> (pkgPath : Bool)
          -> (desc    : PkgDesc)
          -> ResolvedPackage

  ||| A local project with (absolute) path to project's
  ||| directory and parsed `.ipkg` file.
  ||| `pkgPath` should be set to `True` for executable which need
  ||| access to the `IDRIS2_PACKAGE_PATH`: The list of directories
  ||| where Idris packages are installed.
  RLocal  :  (name    : PkgName)
          -> (dir     : Path Abs)
          -> (ipkg    : Path Rel)
          -> (pkgPath : Bool)
          -> (desc    : PkgDesc)
          -> ResolvedPackage

  ||| The *base* library from the Idris2 project.
  Core    : CorePkg -> PkgDesc -> ResolvedPackage

||| True, if the given resolved package represents
||| one of the core packages (`base`, `prelude`, etc.)
export
isCorePackage : ResolvedPackage -> Bool
isCorePackage (RGitHub _ _ _ _ _ _) = False
isCorePackage (RLocal _ _ _ _ _)    = False
isCorePackage (Core _ _)            = True

||| Try to extract the package description from a
||| resolved package.
export
desc : ResolvedPackage -> PkgDesc
desc (RGitHub _ _ _ _ _ d) = d
desc (RLocal _ _ _ _ d)    = d
desc (Core _ d)            = d

namespace PkgDesc
  export
  dependencies : PkgDesc -> List PkgName
  dependencies = map (MkPkgName . pkgname) . depends

||| Extracts the dependencies of a resolved package.
export
dependencies : ResolvedPackage -> List PkgName
dependencies = dependencies . desc

||| Extracts the name of the executable (if any) from
||| a resolved package.
export
executable : ResolvedPackage -> Maybe String
executable = executable . desc

||| Extracts the package name from a resolved package.
export
name : ResolvedPackage -> PkgName
name (RGitHub n _ _ _ _ _) = n
name (RLocal n _ _ _ _)    = n
name (Core c _)            = MkPkgName "\{c}"

||| Extracts the package name from a resolved package.
export
nameStr : ResolvedPackage -> String
nameStr = value . name

||| True, if the given application needs access to the
||| folders where Idris package are installed.
export
usePackagePath : ResolvedPackage -> Bool
usePackagePath (RGitHub _ _ _ _ pp _) = pp
usePackagePath (RLocal _ _ _ pp _)    = pp
usePackagePath _                      = False

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
  packagePath = \{tomlBool pp}
  """

printPair (x, Local dir ipkg pp) =
  """

  [db.\{x}]
  type        = "local"
  path        = "\{dir}"
  ipkg        = "\{ipkg}"
  packagePath = \{tomlBool pp}
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
