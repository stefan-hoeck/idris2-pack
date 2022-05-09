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
query_ e q = mapMaybe id <$> traverse run (keys e.db.packages)
  where run : PkgName -> EitherT PackErr io (Maybe b)
        run n = case q n of
          Nothing => pure Nothing
          Just f  => Just . f <$> resolve e (Pkg n)

fromTpe : QueryType -> PkgName -> ResolvedPackage -> String
fromTpe NameOnly     p rp = p.value
fromTpe ShortDesc    p rp =
  let Just d := desc rp | Nothing => p.value
      Just b := brief d | Nothing => p.value
   in "\{p}\n  \{b}"
fromTpe Dependencies p rp =
  let Just d := desc rp        | Nothing => p.value
      ds@(_ :: _) := d.depends | [] => p.value
   in unlines $  p.value
              :: map (("  " ++) . pkgname) ds

export
query : HasIO io => String -> (e : Env s) -> EitherT PackErr io ()
query n e = do
  ss <- query_ e $ \p => toMaybe (isInfixOf n p.value) (fromTpe e.queryType p)
  putStrLn $ unlines ss
