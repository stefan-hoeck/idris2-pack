module Pack.Runner.Query

import Data.List
import Data.Maybe
import Data.SortedMap
import Data.String
import Idris.Package.Types
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types
import Pack.Runner.Database

%default total

query_ :  HasIO io
       => (e : Env s)
       -> (q : PkgName -> Maybe (ResolvedPackage -> b))
       -> EitherT PackErr io (List b)
query_ e q = mapMaybe id <$> traverse run (keys $ allPackages e)
  where run : PkgName -> EitherT PackErr io (Maybe b)
        run n = case q n of
          Nothing => pure Nothing
          Just f  => Just . f <$> resolve e (Pkg n)

shortDesc : ResolvedPackage -> Maybe String
shortDesc (RGitHub _ _ _ _ d) = d.brief
shortDesc (RIpkg _ d)         = d.brief
shortDesc (RLocal _ _ _ d)    = d.brief
shortDesc Base                = Just "the Idris2 base library"
shortDesc Contrib             = Just "the Idris2 contrib library"
shortDesc Idris2              = Just "the Idris2 API"
shortDesc Linear              = Nothing
shortDesc Network             = Nothing
shortDesc Prelude             = Just "the Idris2 Prelude"
shortDesc Test                = Nothing

deps : ResolvedPackage -> List String
deps (RGitHub _ _ _ _ d) = map pkgname d.depends
deps (RIpkg _ d)         = map pkgname d.depends
deps (RLocal _ _ _ d)    = map pkgname d.depends
deps Base                = []
deps Contrib             = []
deps Idris2              = []
deps Linear              = []
deps Network             = []
deps Prelude             = []
deps Test                = ["contrib"]

prettyDeps : ResolvedPackage -> List String
prettyDeps rp = case deps rp of
  []     => ["Dependencies :"]
  h :: t => "Dependencies : \{h}" :: map (indent 15) t

details : ResolvedPackage -> List String
details (RGitHub name url commit ipkg desc) = [
    "Type         : GitHub project"
  , "URL          : \{url}"
  , "Commit       : \{commit}"
  , "ipkg File    : \{ipkg}"
  ]

details (RIpkg path desc) = [
    "Type         : Local ipkg file"
  , "Path         : \{path}"
  ]

details (RLocal name dir ipkg desc) = [
    "Type         : Local Idris project"
  , "Location     : \{dir}"
  , "ipkg File    : \{ipkg}"
  ]

details Base    = [ "Type           : Idris core package" ]
details Contrib = [ "Type           : Idris core package" ]
details Idris2  = [ "Type           : Idris core package" ]
details Linear  = [ "Type           : Idris core package" ]
details Network = [ "Type           : Idris core package" ]
details Prelude = [ "Type           : Idris core package" ]
details Test    = [ "Type           : Idris core package" ]


fromTpe : QueryType -> PkgName -> ResolvedPackage -> String
fromTpe NameOnly     p rp = p.value

fromTpe ShortDesc    p rp =
  let Just d := shortDesc rp | Nothing => p.value
   in "\{p}\n  \{d}"

fromTpe Dependencies p rp =
  let ds@(_ :: _) := deps rp | [] => p.value
   in unlines $  p.value :: map (indent 2) ds

fromTpe Details p rp = unlines . (p.value ::) . map (indent 2) $ concat [
    toList (("Brief       : " ++) <$> shortDesc rp)
  , details rp
  , prettyDeps rp
  ]

export
query : HasIO io => String -> (e : Env s) -> EitherT PackErr io ()
query n e = do
  ss <- query_ e $ \p => toMaybe (isInfixOf n p.value) (fromTpe e.queryType p)
  putStrLn $ unlines ss
