module Pack.Database.TOML

import Data.SortedMap
import Idris.Package.Types
import Libraries.Utils.Path
import Pack.Core.TOML
import Pack.Core.Types
import Pack.Database.Types

%default total

export
FromTOML MetaCommit where fromTOML = tmap fromString

github : FromTOML c => Value -> Either TOMLErr (Package_ c)
github v = [| GitHub (valAt "url" v)
                     (valAt "commit" v)
                     (valAt "ipkg" v)
                     (optValAt "packagePath" False v) |]

local : Path Abs -> Value -> Either TOMLErr (Package_ c)
local dir v = [| Local (absPathAt "path" dir v)
                       (valAt "ipkg" v)
                       (optValAt "packagePath" False v) |]

export
package : FromTOML c => Path Abs -> Value -> Either TOMLErr (Package_ c)
package dir v = valAt {a = String} "type" v >>=
  \case "github" => github v
        "local"  => local dir v
        _        => Left $ WrongType ["type"] "Package Type"

||| URL of the Idris repository
export
idrisRepo : URL
idrisRepo = "https://github.com/idris-lang/Idris2.git"

export
db : FromTOML c => Path Abs -> Value -> Either TOMLErr DB
db dir v = [| MkDB (optValAt "idris2.url" idrisRepo v)
                   (valAt "idris2.commit" v)
                   (valAt "idris2.version" v)
                   (valAt' (sortedMap package dir) "db" (Just empty) v) |]
