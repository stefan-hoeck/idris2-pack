module Pack.Config.TOML

import Data.List1
import Data.String

import Libraries.Utils.Path

import Pack.Config.Types
import Pack.Core.TOML
import Pack.Core.Types
import Pack.Database.TOML
import Pack.Database.Types

%default total

||| Initial configuration.
export
config : (dir : Path) -> Value -> Either TOMLErr (Config Nothing)
config dir v =
  [| MkConfig (pure dir)
              (valAt "collection" v)
              (optValAt "idris2.scheme" (parse "scheme") v)
              (optValAt "idris2.bootstrap" False v)
              (optValAt "install.with-src" False v)
              (optValAt "install.safety-prompt" True v)
              (optValAt "install.libs" [] v)
              (optValAt "install.apps" [] v)
              (optValAt "custom" empty v)
              (pure NameOnly)
              (pure ())
  |]
