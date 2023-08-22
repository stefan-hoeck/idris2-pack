module Pack.Database.Types

import Core.Name.Namespace
import Data.List1
import Data.List.Elem
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
  Latest : Branch -> MetaCommit
  Fetch  : Branch -> MetaCommit

public export
FromString MetaCommit where
  fromString s = case forget $ split (':' ==) s of
    ["latest",branch]       => Latest $ MkBranch branch
    ["fetch-latest",branch] => Fetch $ MkBranch branch
    _                       => MC $ MkCommit s

export
Interpolation MetaCommit where
  interpolate (Latest b) = "latest:\{b}"
  interpolate (Fetch b)  = "fetch-latest:\{b}"
  interpolate (MC c)     = "\{c}"

export
toLatest : MetaCommit -> MetaCommit
toLatest (MC x) = Latest (MkBranch x.value)
toLatest x      = x

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
  | Papers
  | IdrisApi

||| The list of core packages.
public export
corePkgs : List CorePkg
corePkgs = [Prelude, Base, Contrib, Linear, Network, Test, Papers, IdrisApi]

export
Interpolation CorePkg where
  interpolate Prelude  = "prelude"
  interpolate Base     = "base"
  interpolate Contrib  = "contrib"
  interpolate Linear   = "linear"
  interpolate Network  = "network"
  interpolate Test     = "test"
  interpolate Papers   = "papers"
  interpolate IdrisApi = "idris2"

export
Cast CorePkg Body where
  cast Prelude  = "prelude"
  cast Base     = "base"
  cast Contrib  = "contrib"
  cast Linear   = "linear"
  cast Network  = "network"
  cast Test     = "test"
  cast Papers   = "papers"
  cast IdrisApi = "idris2"

export %inline
Cast CorePkg (Path Rel) where
  cast c = PRel [< cast c]

||| Package name of a core package.
export
corePkgName : CorePkg -> PkgName
corePkgName = MkPkgName . interpolate

||| `.ipkg` file name corrsponding to a core package.
export
coreIpkgFile : CorePkg -> Body
coreIpkgFile IdrisApi = "idris2api.ipkg"
coreIpkgFile c        = cast c <+> ".ipkg"

||| Relative path to the `.ipkg` file corrsponding to a core package
||| (in the Idris2 project).
export
coreIpkgPath : CorePkg -> File Rel
coreIpkgPath IdrisApi = MkF neutral "idris2api.ipkg"
coreIpkgPath c        = MkF (neutral /> "libs" //> c) (coreIpkgFile c)

||| Try to convert a string to a core package.
export
readCorePkg : String -> Maybe CorePkg
readCorePkg "prelude" = Just Prelude
readCorePkg "base"    = Just Base
readCorePkg "contrib" = Just Contrib
readCorePkg "linear"  = Just Linear
readCorePkg "network" = Just Network
readCorePkg "test"    = Just Test
readCorePkg "papers"  = Just Papers
readCorePkg "idris2"  = Just IdrisApi
readCorePkg _         = Nothing

||| True, if the given string corresponds to one of the core packges.
export
isCorePkg : String -> Bool
isCorePkg = isJust . readCorePkg

--------------------------------------------------------------------------------
--          Packages
--------------------------------------------------------------------------------

||| Description of a Git or local Idris package in the
||| package database.
|||
||| Note: This does not contain the package name, as it
||| will be paired with its name in a `SortedMap`.
public export
data Package_ : (c : Type) -> Type where
  ||| A Git repository, given as the package's URL,
  ||| commit (hash or tag), and name of `.ipkg` file to use.
  ||| `pkgPath` should be set to `True` for executables which need
  ||| access to the `IDRIS2_PACKAGE_PATH`: The list of directories
  ||| where Idris packages are installed.
  Git :  (url      : URL)
      -> (commit   : c)
      -> (ipkg     : File Rel)
      -> (pkgPath  : Bool)
      -> (testIpkg : Maybe (File Rel))
      -> Package_ c

  ||| A local Idris project given as an absolute path to a local
  ||| directory, and `.ipkg` file to use.
  ||| `pkgPath` should be set to `True` for executable which need
  ||| access to the `IDRIS2_PACKAGE_PATH`: The list of directories
  ||| where Idris packages are installed.
  Local  :  (dir      : Path Abs)
         -> (ipkg     : File Rel)
         -> (pkgPath  : Bool)
         -> (testIpkg : Maybe (File Rel))
         -> Package_ c

  ||| A core package of the Idris2 project
  Core   : (core : CorePkg) -> Package_ c

export
Functor Package_ where
  map f (Git u c i p t) = Git u (f c) i p t
  map f (Local d i p t) = Local d i p t
  map f (Core c)        = Core c

export
traverse : Applicative f => (URL -> a -> f b) -> Package_ a -> f (Package_ b)
traverse g (Git u c i p t) = (\c' => Git u c' i p t) <$> g u c
traverse _ (Local d i p t)    = pure $ Local d i p t
traverse _ (Core c)           = pure $ Core c

||| An alias for `Package_ Commit`: A package description with
||| meta commits already resolved.
public export
0 Package : Type
Package = Package_ Commit

||| An alias for `Package_ MetaCommit`: A package description where
||| the commit might still contain meta information.
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
Uninhabited (IsCore $ Git {}) where
  uninhabited _ impossible

||| Decides, if the given package represents
||| one of the core packages (`base`, `prelude`, etc.)
export
isCore : (p : Package) -> Dec (IsCore p)
isCore (Core {})  = Yes ItIsCore
isCore (Git {})   = No absurd
isCore (Local {}) = No absurd

||| Proof that a package is a local package
public export
data IsLocal : Package -> Type where
  ItIsLocal : IsLocal (Local {})

export
Uninhabited (IsLocal $ Core {}) where
  uninhabited _ impossible

export
Uninhabited (IsLocal $ Git {}) where
  uninhabited _ impossible

||| Decides, if the given package represents
||| a local package.
export
isLocal : (p : Package) -> Dec (IsLocal p)
isLocal (Core {})  = No absurd
isLocal (Git {})   = No absurd
isLocal (Local {}) = Yes ItIsLocal

||| Proof that a package is a Git package
public export
data IsGit : Package -> Type where
  ItIsGit : IsGit (Git {})

export
Uninhabited (IsGit $ Core {}) where
  uninhabited _ impossible

export
Uninhabited (IsGit $ Local {}) where
  uninhabited _ impossible

||| Decides, if the given package represents
||| a Git package.
export
isGit : (p : Package) -> Dec (IsGit p)
isGit (Core {})  = No absurd
isGit (Git {})   = Yes ItIsGit
isGit (Local {}) = No absurd

||| True, if the given application needs access to the
||| folders where Idris package are installed.
export
usePackagePath : Package_ c -> Bool
usePackagePath (Git _ _ _ pp _) = pp
usePackagePath (Local _ _ pp _) = pp
usePackagePath (Core _)         = False

||| Absolute path to the `.ipkg` file of a package.
export
ipkg : (dir : Path Abs) -> Package -> File Abs
ipkg dir (Git _ _ i _ _) = toAbsFile dir i
ipkg dir (Local _ i _ _) = toAbsFile dir i
ipkg dir (Core c)        = toAbsFile dir (coreIpkgPath c)

--------------------------------------------------------------------------------
--          Resolved Packages
--------------------------------------------------------------------------------

||| Installation status of an Idris package. Local packages can be
||| `Outdated`, if some of their source files contain changes newer
||| a timestamp created during package installation.
public export
data PkgStatus : Package -> Type where
  Missing     : PkgStatus p
  Installed   : (withDocs : Bool) -> PkgStatus p
  Outdated    : (0 isLocal : IsLocal p) => PkgStatus p

||| A resolved library, which was cloned from a Git repo
||| or looked up in the local file system. This comes with
||| a fully parsed `PkgDesc` (representing the `.ipkg` file).
public export
record ResolvedLib t where
  constructor RL
  pkg     : Package
  name    : PkgName
  desc    : Desc t
  status  : PkgStatus pkg
  deps    : List (DPair Package PkgStatus)

namespace ResolvedLib
  ||| Extracts the package name from a resolved library.
  export %inline
  nameStr : ResolvedLib t -> String
  nameStr = value . name

  ||| Change the type-level tag of a resolved library.
  export %inline
  reTag : ResolvedLib s -> Desc t -> ResolvedLib t
  reTag rl d = {desc := d} rl

  ||| Extracts the dependencies of a resolved library.
  export
  dependencies : ResolvedLib t -> List PkgName
  dependencies rp = dependencies rp.desc

  ||| Check if the given library is installed
  export
  isInstalled : ResolvedLib t -> Bool
  isInstalled rl = case rl.status of
    Missing => False
    _       => True

namespace AppStatus
  ||| Installation status of an Idris app. Local apps can be
  ||| `Outdated`, if some of their source files contain changes newer
  ||| a timestamp created during package installation.
  public export
  data AppStatus : Package -> Type where
    ||| The app has not been compiled and is therfore missing
    Missing      :  AppStatus p

    ||| The app has been built but is not on the `PATH`.
    Installed    :  AppStatus p

    ||| The app has been built and a wrapper script has been added
    ||| to `$PACK_DIR/bin`, so it should be on the `PATH`.
    BinInstalled :  AppStatus p

    ||| The local app has changes in its source files, which have
    ||| not yet been included in the installed version.
    Outdated     :  (0 isLocal : IsLocal p) => AppStatus p

||| A resolved application, which was cloned from a Git repo
||| or looked up in the local file system. This comes with
||| a fully parsed `PkgDesc` (representing the `.ipkg` file).
public export
record ResolvedApp t where
  constructor RA
  pkg     : Package
  name    : PkgName
  desc    : Desc t
  status  : AppStatus pkg
  exec    : Body
  deps    : List (DPair Package PkgStatus)

namespace ResolveApp
  ||| Extracts the package name from a resolved application.
  export %inline
  nameStr : ResolvedApp t -> String
  nameStr = value . name

  ||| Extracts the dependencies of a resolved application.
  export
  dependencies : ResolvedApp t -> List PkgName
  dependencies rp = dependencies rp.desc

  ||| Change the type-level tag of a resolved application.
  export %inline
  reTag : ResolvedApp s -> Desc t -> ResolvedApp t
  reTag rl d = {desc := d} rl

  ||| True, if the given application needs access to the
  ||| folders where Idris package are installed.
  export %inline
  usePackagePath : ResolvedApp t -> Bool
  usePackagePath = usePackagePath . pkg

||| Either a resolved library or application tagged with the given tag.
||| This is to be used in build plans, so applications come with the
||| additional info whether we want to install a wrapper script or not.
public export
data LibOrApp : (t,s : PkgDesc -> Type) -> Type where
  Lib : ResolvedLib t -> LibOrApp t s
  App : (withWrapperScript : Bool) -> ResolvedApp s -> LibOrApp t s

namespace LibOrApp
  ||| Extract the dependencies of a resolved library or application.
  export
  dependencies : LibOrApp t s -> List PkgName
  dependencies (Lib x)   = dependencies x
  dependencies (App _ x) = dependencies x

  ||| Extract the package of a resolved library or application.
  export
  pkg : LibOrApp t s -> Package
  pkg (Lib x)   = x.pkg
  pkg (App _ x) = x.pkg

  ||| Extract the description of a resolved library or application.
  export
  desc : LibOrApp t t -> Desc t
  desc (Lib x)   = x.desc
  desc (App _ x) = x.desc

  ||| Extract the package name of a resolved library or application.
  export
  name : LibOrApp t s -> PkgName
  name (Lib x)   = x.name
  name (App _ x) = x.name

  ||| Try to extract the package name of a resolved library.
  export
  libName : LibOrApp t s -> Maybe PkgName
  libName (Lib x)   = Just x.name
  libName (App _ _) = Nothing

--------------------------------------------------------------------------------
--          Package Database
--------------------------------------------------------------------------------

||| DB used for building packages. This includes
||| the Idris commit to use, together with a curated list of
||| known packages.
public export
record DB_ c where
  constructor MkDB
  idrisURL     : URL
  idrisCommit  : c
  idrisVersion : PkgVersion
  packages     : SortedMap PkgName (Package_ c)

export
Functor DB_ where
  map f db = {
      idrisCommit $= f
    , packages    $= map (map f)
    } db

public export
0 DB : Type
DB = DB_ Commit

public export
0 MetaDB : Type
MetaDB = DB_ MetaCommit

||| Effectfully convert package descriptions in a DB
export
traverseDB :
     {auto _ : Applicative f}
  -> (URL -> a -> f b)
  -> DB_ a
  -> f (DB_ b)
traverseDB g db =
  let ic   := g db.idrisURL db.idrisCommit
      pkgs := traverse (traverse g) db.packages
   in [| adj ic pkgs |]
    where
      adj : b -> SortedMap PkgName (Package_ b) -> DB_ b
      adj ic cb = {idrisCommit := ic, packages := cb} db

tomlBool : Bool -> String
tomlBool True  = "true"
tomlBool False = "false"

testPath : Maybe (File Rel) -> List String
testPath Nothing  = []
testPath (Just x) = [ "test        = \{quote x}" ]

-- we need to print `Git` packages as `"github"` at
-- least for the time being for reasons of compatibility
printPair : (PkgName,Package) -> List String
printPair (x, Git url commit ipkg pp t) =
  [ "[db.\{x}]"
  , "type        = \"github\""
  , "url         = \{quote url}"
  , "commit      = \{quote commit}"
  , "ipkg        = \{quote ipkg}"
  , "packagePath = \{tomlBool pp}"
  ] ++ testPath t

printPair (x, Local dir ipkg pp t) =
  [ "[db.\{x}]"
  , "type        = \"local\""
  , "path        = \{quote dir}"
  , "ipkg        = \{quote ipkg}"
  , "packagePath = \{tomlBool pp}"
  ] ++ testPath t

printPair (x, Core c) =
  [ "[db.\{x}]"
  , "type        = \"core\""
  ]

||| Convert a package collection to a valid TOML string.
export
printDB : DB -> String
printDB (MkDB u c v db) =
  let header := """
        [idris2]
        url     = "\{u}"
        version = "\{v}"
        commit  = "\{c}"
        """
   in unlines $ header :: (SortedMap.toList db >>= \p => "" :: printPair p)

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
corePkgsTest Papers   = %search
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
corePkgRoundTrip Papers   = Refl
corePkgRoundTrip IdrisApi = Refl
