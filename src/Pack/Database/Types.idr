module Pack.Database.Types

import Core.Name.Namespace
import Data.List1
import Data.List.Elem
import Data.SortedMap
import Data.String
import Idris.Package.Types
import Pack.Core.Ipkg
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
--          Core Packages
--------------------------------------------------------------------------------

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

public export
corePkgs : List CorePkg
corePkgs = [Prelude, Base, Contrib, Linear, Network, Test, IdrisApi]

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
Cast CorePkg Body where
  cast Prelude  = "prelude"
  cast Base     = "base"
  cast Contrib  = "contrib"
  cast Linear   = "linear"
  cast Network  = "network"
  cast Test     = "test"
  cast IdrisApi = "idris2"

export %inline
Cast CorePkg (Path Rel) where
  cast c = PRel [< cast c]

export
corePkgName : CorePkg -> PkgName
corePkgName = MkPkgName . interpolate

export
coreIpkgFile : CorePkg -> Body
coreIpkgFile IdrisApi = "idris2api.ipkg"
coreIpkgFile c        = cast c <+> ".ipkg"

export
coreIpkgPath : CorePkg -> File Rel
coreIpkgPath IdrisApi = MkF neutral "idris2api.ipkg"
coreIpkgPath c        = MkF (neutral /> "libs" //> c) (coreIpkgFile c)

export
readCorePkg : String -> Maybe CorePkg
readCorePkg "prelude" = Just Prelude
readCorePkg "base"    = Just Base
readCorePkg "contrib" = Just Contrib
readCorePkg "linear"  = Just Linear
readCorePkg "network" = Just Network
readCorePkg "test"    = Just Test
readCorePkg "idris2"  = Just IdrisApi
readCorePkg _         = Nothing

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
  ||| `pkgPath` should be set to `True` for executables which need
  ||| access to the `IDRIS2_PACKAGE_PATH`: The list of directories
  ||| where Idris packages are installed.
  GitHub :  (url     : URL)
         -> (commit  : c)
         -> (ipkg    : File Rel)
         -> (pkgPath : Bool)
         -> Package_ c

  ||| A local Idris project given as an absolute path to a local
  ||| directory, and `.ipkg` file to use.
  ||| `pkgPath` should be set to `True` for executable which need
  ||| access to the `IDRIS2_PACKAGE_PATH`: The list of directories
  ||| where Idris packages are installed.
  Local  :  (dir     : Path Abs)
         -> (ipkg    : File Rel)
         -> (pkgPath : Bool)
         -> Package_ c

  ||| A core package of the Idris2 project
  Core   : (core : CorePkg) -> Package_ c

public export
0 Package : Type
Package = Package_ Commit

public export
0 UserPackage : Type
UserPackage = Package_ MetaCommit

||| Proof that a package is a core package
public export
data IsCore : Package -> Type where
  ItIsCore : IsCore (Core c)

export
Uninhabited (IsCore $ Local {}) where
  uninhabited _ impossible

export
Uninhabited (IsCore $ GitHub {}) where
  uninhabited _ impossible

||| Decides, if the given package represents
||| one of the core packages (`base`, `prelude`, etc.)
export
isCore : (p : Package) -> Dec (IsCore p)
isCore (Core {})   = Yes ItIsCore
isCore (GitHub {}) = No absurd
isCore (Local {})  = No absurd

||| Proof that a package is a local package
public export
data IsLocal : Package -> Type where
  ItIsLocal : IsLocal (Local {})

export
Uninhabited (IsLocal $ Core {}) where
  uninhabited _ impossible

export
Uninhabited (IsLocal $ GitHub {}) where
  uninhabited _ impossible

||| Decides, if the given package represents
||| a local package.
export
isLocal : (p : Package) -> Dec (IsLocal p)
isLocal (Core {})   = No absurd
isLocal (GitHub {}) = No absurd
isLocal (Local {})  = Yes ItIsLocal

||| Proof that a package is a GitHub package
public export
data IsGitHub : Package -> Type where
  ItIsGitHub : IsGitHub (GitHub {})

export
Uninhabited (IsGitHub $ Core {}) where
  uninhabited _ impossible

export
Uninhabited (IsGitHub $ Local {}) where
  uninhabited _ impossible

||| Decides, if the given package represents
||| a package on GitHub.
export
isGitHub : (p : Package) -> Dec (IsGitHub p)
isGitHub (Core {})   = No absurd
isGitHub (GitHub {}) = Yes ItIsGitHub
isGitHub (Local {})  = No absurd

||| True, if the given application needs access to the
||| folders where Idris package are installed.
export
usePackagePath : Package_ c -> Bool
usePackagePath (GitHub _ _ _ pp) = pp
usePackagePath (Local _ _ pp)    = pp
usePackagePath (Core _)          = False

||| Absolute path to the `.ipkg` file of a package.
export
ipkg : (dir : Path Abs) -> Package -> File Abs
ipkg dir (GitHub _ _ i _) = toAbsFile dir i
ipkg dir (Local _ i _)    = toAbsFile dir i
ipkg dir (Core c)         = toAbsFile dir (coreIpkgPath c)

--------------------------------------------------------------------------------
--          Resolved Packages
--------------------------------------------------------------------------------

public export
data PkgStatus : Package -> Type where
  Missing   :  PkgStatus p
  Installed :  PkgStatus p
  Outdated  :  (0 isLocal : IsLocal p) => PkgStatus p

||| A resolved library, which was downloaded from GitHub
||| or looked up in the local file system. This comes with
||| a fully parsed `PkgDesc` (representing the `.ipkg` file).
public export
record ResolvedLib t where
  constructor RL
  pkg     : Package
  name    : PkgName
  desc    : Desc t
  status  : PkgStatus pkg

namespace ResolveLib
  ||| Extracts the package name from a resolved library.
  export %inline
  nameStr : ResolvedLib t -> String
  nameStr = value . name

  export %inline
  reTag : ResolvedLib s -> Desc t -> ResolvedLib t
  reTag rl d = {desc := d} rl

  ||| Extracts the dependencies of a resolved library.
  export
  dependencies : ResolvedLib t -> List PkgName
  dependencies rp = dependencies rp.desc

||| A resolved application, which was downloaded from GitHub
||| or looked up in the local file system. This comes with
||| a fully parsed `PkgDesc` (representing the `.ipkg` file).
public export
record ResolvedApp t where
  constructor RA
  pkg     : Package
  name    : PkgName
  desc    : Desc t
  status  : PkgStatus pkg
  exec    : Body

namespace ResolveApp
  ||| Extracts the package name from a resolved application.
  export %inline
  nameStr : ResolvedApp t -> String
  nameStr = value . name

  ||| Extracts the dependencies of a resolved application.
  export
  dependencies : ResolvedApp t -> List PkgName
  dependencies rp = dependencies rp.desc

  export %inline
  reTag : ResolvedApp s -> Desc t -> ResolvedApp t
  reTag rl d = {desc := d} rl

  ||| True, if the given application needs access to the
  ||| folders where Idris package are installed.
  export %inline
  usePackagePath : ResolvedApp t -> Bool
  usePackagePath = usePackagePath . pkg

public export
0 LibOrApp : (PkgDesc -> Type) -> Type
LibOrApp t = Either (ResolvedLib t) (ResolvedApp t)

namespace LibOrApp
  export
  dependencies : LibOrApp t -> List PkgName
  dependencies = either dependencies dependencies

  export
  pkg : LibOrApp t -> Package
  pkg (Left x) = x.pkg
  pkg (Right x) = x.pkg

  export
  desc : LibOrApp t -> Desc t
  desc (Left x) = x.desc
  desc (Right x) = x.desc

  export
  name : LibOrApp t -> PkgName
  name (Left x) = x.name
  name (Right x) = x.name

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

printPair (x, Core c) =
  """

  [db.\{x}]
  type        = "core"
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

--------------------------------------------------------------------------------
--          Tests
--------------------------------------------------------------------------------

-- make sure no core package was forgotten
0 corePkgsTest : (c : CorePkg) -> Elem c Types.corePkgs
corePkgsTest Prelude  = %search
corePkgsTest Base     = %search
corePkgsTest Contrib  = %search
corePkgsTest Linear   = %search
corePkgsTest Network  = %search
corePkgsTest Test     = %search
corePkgsTest IdrisApi = %search

-- all core packages should be parsable from their
-- interpolation string
0 corePkgRoundTrip : (c : CorePkg) -> readCorePkg (interpolate c) === Just c
corePkgRoundTrip Prelude  = Refl
corePkgRoundTrip Base     = Refl
corePkgRoundTrip Contrib  = Refl
corePkgRoundTrip Linear   = Refl
corePkgRoundTrip Network  = Refl
corePkgRoundTrip Test     = Refl
corePkgRoundTrip IdrisApi = Refl
