||| This module contains the code for the *micropack*
||| application used for first-time users of Idris2 and
||| pack.
module MicroPack

import Data.IORef
import Data.Maybe
import Data.SortedMap
import Data.String
import Pack.Config
import Pack.Core
import Pack.Database
import Pack.Runner.Install
import System

%default total

microInit :  (scheme : String)
          -> (db     : DBName)
          ->  MetaConfig
microInit scheme db = MkConfig {
    collection    = db
  , idrisURL      = Nothing
  , idrisCommit   = Nothing
  , packURL       = Nothing
  , packCommit    = Nothing
  , scheme        = fromString scheme
  , bootstrap     = True
  , safetyPrompt  = False
  , gcPrompt      = False
  , warnDepends   = True
  , whitelist     = []
  , withSrc       = True
  , withDocs      = False
  , useKatla      = False
  , withIpkg      = None
  , rlwrap        = DoNotUseRlwrap
  , autoLibs      = []
  , autoApps      = []
  , autoLoad      = NoPkgs
  , custom        = empty
  , queryType     = NameOnly
  , logLevel      = Build
  , codegen       = Chez
  , output        = "_tmppack"
  , levels        = empty
  }

covering
main : IO ()
main = run $ do
  dir     <- getPackDir
  td      <- mkTmpDir
  cache   <- emptyCache
  mkDir packDir
  defCol  <- defaultColl
  args    <- getArgs
  scheme  <- fromMaybe "scheme" <$> getEnv "SCHEME"
  linebuf <- getLineBufferingCmd

  let db   = case args of
        [_,n] => either (const defCol) id $ readDBName n
        _     => defCol

      conf = microInit scheme db

  -- initialize `$HOME/.pack/user/pack.toml`
  write (MkF (packDir /> "user") packToml) (initToml scheme db)

  finally (rmDir tmpDir) $ idrisEnv conf True >>= update
