module Pack.Database.TOML

import Data.SortedMap
import Idris.Package.Types
import Libraries.Utils.Path
import Pack.Core.TOML
import Pack.Core.Types
import Pack.Database.Types

%default total

github : Value -> Either TOMLErr Package
github v = [| GitHub (valAt "url" v) (valAt "commit" v) (valAt "ipkg" v) |]

local : Value -> Either TOMLErr Package
local v = [| Local (valAt "path" v) (valAt "ipkg" v) |]

package : Value -> Either TOMLErr Package
package v = valAt {a = String} "type" v >>=
  \case "github" => github v
        "local"  => local v
        _        => Left $ WrongType ["type"] "Package Type"

export %inline
FromTOML Package where
  fromTOML = package

export
FromTOML DB where
  fromTOML v = [| MkDB (valAt "idris2.commit" v)
                       (valAt "idris2.version" v)
                       (valAt "db" v) |]
