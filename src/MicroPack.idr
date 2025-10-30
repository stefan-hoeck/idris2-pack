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
microInit scheme db =
  MkConfig
    { collection        = db
    , idrisURL          = Nothing
    , idrisCommit       = Nothing
    , allIdrisCommits   = []
    , packURL           = Nothing
    , packCommit        = Nothing
    , scheme            = fromString scheme
    , bootstrap         = True
    , bootstrapStage3   = True
    , safetyPrompt      = False
    , gcPrompt          = False
    , gcPurge           = False
    , warnDepends       = True
    , skipTests         = False
    , whitelist         = []
    , withSrc           = True
    , withDocs          = False
    , useKatla          = False
    , withIpkg          = None
    , rlwrap            = DoNotUseRlwrap
    , extraArgs         = []
    , autoLibs          = []
    , autoApps          = []
    , autoLoad          = NoPkgs
    , custom            = empty
    , queryType         = NameOnly
    , logLevel          = Build
    , codegen           = Default
    , output            = "_tmppack"
    , levels            = empty
    , gitInit           = False
    }

covering
main : IO ()
main = run $ do
  dirs    <- getPackDirs
  cache   <- emptyCache
  mkDir dirs.user
  mkDir dirs.state
  mkDir dirs.cache
  mkDir dirs.bin
  withTmpDir $ do
    defCol  <- defaultColl
    args    <- getArgs
    scheme  <- fromMaybe "scheme" <$> getEnv "SCHEME"
    linebuf <- getLineBufferingCmd

    let db   = case args of
          [_,n] => either (const defCol) id $ readDBName n
          _     => defCol

        conf = microInit scheme db

    -- initialize `$PACK_USER_DIR/pack.toml` and `$PACK_STATE_DIR/pack.toml`
    when !(fileMissing globalPackToml) $
      write globalPackToml (initToml scheme)
    write collectionToml (collectionTomlContent db)

    idrisEnv conf True >>= update
