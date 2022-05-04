||| This module contains the code for the *micropack*
||| application used for first-time users of Idris2 and
||| pack.
module MicroPack

import Data.Maybe
import Data.String
import Libraries.Utils.Path
import Pack.CmdLn.Env
import Pack.CmdLn.Types
import Pack.Core
import Pack.Runner.Install
import System

%default total

microInit :  (dir    : Path)
          -> (scheme : String)
          -> (db     : DBName)
          ->  Config Nothing
microInit dir scheme db = MkConfig {
    cmd           = SwitchRepo db
  , packDir       = dir
  , dbVersion     = db
  , scheme        = parse scheme
  , db            = ()
  , bootstrap     = True
  }

covering
main : IO ()
main = run $ do
  dir    <- packDir
  mkDir dir
  [_,db] <- getArgs | as => throwE (InvalidArgs as)
  scheme <- fromMaybe "scheme" <$> getEnv "SCHEME"
  let conf = microInit dir scheme (MkDBName db)
  updateDB conf
  idrisEnv conf >>= switchCollection
