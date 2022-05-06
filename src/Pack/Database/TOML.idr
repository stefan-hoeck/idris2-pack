module Pack.Database.TOML

import Data.SortedMap as M
import Idris.Package.Types
import Libraries.Utils.Path
import Pack.Core.TOML
import Pack.Core.Types
import Pack.Database.Types

%default total

github : String -> Value -> Either TOMLErr (PkgName,Package)
github n v = prefixKey n $ map (MkPkgName n,)
  [| GitHub (pure $ MkPkgName n)
            (valAt "url" v)
            (valAt "commit" v)
            (valAt "ipkg" v) |]

local : String -> Value -> Either TOMLErr (PkgName,Package)
local n v = prefixKey n $ map (MkPkgName n,)
  [| Local (pure $ MkPkgName n)
           (valAt "path" v)
           (valAt "ipkg" v) |]

package : String -> Value -> Either TOMLErr (PkgName,Package)
package n v = case valAt {a = String} "type" v of
  Right "github" => github n v
  Right "local"  => local n v
  Right _        => Left $ WrongType [n,"type"] "Package Type"
  Left  err      => prefixKey n (Left err)

githubMap : (val  : Value) -> Either TOMLErr (SortedMap PkgName Package)
githubMap (VTable m) = M.fromList <$> traverse (uncurry github) (M.toList m)
githubMap _          = Left $ WrongType [] "Table"

packageMap : (val  : Value) -> Either TOMLErr (SortedMap PkgName Package)
packageMap (VTable m) = M.fromList <$> traverse (uncurry package) (M.toList m)
packageMap _          = Left $ WrongType [] "Table"

export
FromTOML (SortedMap PkgName Package) where
  fromTOML = packageMap

[GitHub] FromTOML (SortedMap PkgName Package) where
  fromTOML = githubMap

export
FromTOML DB where
  fromTOML v =
    [| MkDB (valAt "idris2.commit" v)
            (valAt "idris2.version" v)
            (valAt @{GitHub} "db" v) |]
