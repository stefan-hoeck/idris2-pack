module Pack.Database.TOML

import Data.SortedMap
import Idris.Package.Types
import Libraries.Utils.Path
import Text.TOML
import Pack.Core.TOML
import Pack.Core.Types
import Pack.Database.Types

%default total

export
FromTOML MetaCommit where fromTOML = tmap fromString

git : FromTOML c => File Abs -> TomlValue -> Either TOMLErr (Package_ c)
git f v =
  [| Git
       (valAt "url" f v)
       (valAt "commit" f v)
       (valAt "ipkg" f v)
       (optValAt "packagePath" f False v)
       (maybeValAt "test" f v)
  |]

local : File Abs -> TomlValue -> Either TOMLErr (Package_ c)
local f v =
  [| Local
       (valAt "path" f v)
       (valAt "ipkg" f v)
       (optValAt "packagePath" f False v)
       (maybeValAt "test" f v)
  |]

package : FromTOML c => File Abs -> TomlValue -> Either TOMLErr (Package_ c)
package f v = valAt {a = String} "type" f v >>=
  \case
    "git"    => git f v
    "github" => git f v -- for compatibility
    "local"  => local f v
    _        => Left $ WrongType ["type"] "Package Type"

export %inline
FromTOML c => FromTOML (Package_ c) where fromTOML = package

||| URL of the Idris repository
export
idrisRepo : URL
idrisRepo = "https://github.com/idris-lang/Idris2.git"

export
FromTOML MetaDB where
  fromTOML f v =
    [| MkDB
         (optValAt "idris2.url" f idrisRepo v)
         (valAt "idris2.commit" f v)
         (valAt "idris2.version" f v)
         (optValAt "db" f empty v)
    |]
