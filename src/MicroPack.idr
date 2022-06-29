||| This module contains the code for the *micropack*
||| application used for first-time users of Idris2 and
||| pack.
module MicroPack

import Data.Maybe
import Data.SortedMap
import Data.String
import Pack.Config.Env
import Pack.Config.TOML
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types
import Pack.Runner.Install
import System

%default total

microInit :  (dir    : Path Abs)
          -> (scheme : String)
          -> (db     : DBName)
          ->  Config Nothing
microInit dir scheme db = MkConfig {
    packDir       = dir
  , collection    = db
  , scheme        = fromString scheme
  , bootstrap     = True
  , safetyPrompt  = True
  , withSrc       = True
  , withDocs      = False
  , useKatla      = False
  , withIpkg      = None
  , rlwrap        = False
  , autoLibs      = []
  , autoApps      = ["pack"]
  , custom        = empty
  , queryType     = NameOnly
  , logLevel      = Info
  , codegen       = Chez
  , db            = ()
  }

covering
main : IO ()
main = run $ do
  dir     <- packDir
  mkDir dir
  defCol  <- defaultColl dir
  args    <- getArgs
  scheme  <- fromMaybe "scheme" <$> getEnv "SCHEME"

  let db   = case args of
        [_,n] => either (const defCol) id $ readDBName n
        _     => defCol

      conf = microInit dir scheme db

  -- initialize `$HOME/.pack/user/pack.toml`
  write (MkF (dir /> "user") packToml) (initToml scheme db)

  finally (rmDir $ tmpDir conf) $ do
    e <- idrisEnv conf
    install e [(Bin, "pack")]
    links e
