||| This module contains the code for the *micropack*
||| application used for first-time users of Idris2 and
||| pack.
module MicroPack

import Data.Maybe
import Data.SortedMap
import Data.String
import Libraries.Utils.Path
import Pack.Config.Env
import Pack.Config.TOML
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types
import Pack.Runner.Install
import System

%default total

microInit :  (dir    : Path)
          -> (scheme : String)
          -> (db     : DBName)
          ->  Config Nothing
microInit dir scheme db = MkConfig {
    packDir       = dir
  , collection    = db
  , scheme        = parse scheme
  , bootstrap     = True
  , safetyPrompt  = True
  , switchDB      = True
  , withSrc       = True
  , withIpkg      = Nothing
  , rlwrap        = False
  , autoLibs      = []
  , autoApps      = ["pack"]
  , custom        = empty
  , queryType     = NameOnly
  , logLevel      = Info
  , db            = ()
  }

covering
main : IO ()
main = run $ do
  dir     <- packDir
  [_,db'] <- getArgs | as => throwE (InvalidArgs as)
  scheme  <- fromMaybe "scheme" <$> getEnv "SCHEME"

  let db   = MkDBName db'
      conf = microInit dir scheme db

  -- initialize `$HOME/.pack/user/pack.toml`
  write (packToml dir) (initToml scheme db)

  updateDB conf
  ignore $ idrisEnv conf
