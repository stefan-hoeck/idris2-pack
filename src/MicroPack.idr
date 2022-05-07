||| This module contains the code for the *micropack*
||| application used for first-time users of Idris2 and
||| pack.
module MicroPack

import Data.Maybe
import Data.SortedMap
import Data.String
import Libraries.Utils.Path
import Pack.Config.Env
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
  , withSrc       = True
  , autoLibs      = []
  , autoApps      = ["pack"]
  , custom        = empty
  , db            = ()
  }

packToml : (scheme : String) -> (db : DBName) -> String
packToml scheme db = """
  # The package collection to use
  collection = "\{db}"

  [install]

  # Whether to install packages together with their
  # sources or not. This is mainly useful for programmers
  # who have set their editor up with some *go to definition*
  # functionality (for instance by using idris2-lsp with neovim).
  with-src   = true

  # Must-have libraries. These will be installed automatically
  # when using a new package collection.
  # libs       = [ "toml", "elab-util" ]

  # Must-have applications. These will be installed automatically
  # when using a new package collection.
  apps       = [ "pack" ]

  [idris2]

  # Whether to build Idris2 with its bootstrap compiler.
  # Bootstrapping takes longer than building with an existing
  # Idris2 installation, but it will work even if the existing
  # Idris2 compiler is outdated.
  bootstrap  = false

  # Name or path to the scheme executable to use.
  scheme     = "\{scheme}"

  # Below are some examples for custom packages

  # A local package to be available with all
  # package collections.
  # [custom.all.chem]
  # type = "local"
  # path = "/data/idris/chem"
  # ipkg = "chem.ipkg"

  # A package on GitHub to be available with all
  # package collections.
  # [custom.all.foo]
  # type = "github"
  # path = "https://github.com/bar/foo"
  # ipkg = "foo.ipkg"

  # Override library `toml` from package collection `nightly-220503`
  # by using a custom commit hash.
  # [custom.nightly-220503.toml]
  # type   = "github"
  # url    = "https://github.com/cuddlefishie/toml-idr"
  # commit = "eb7a146f565276f82ebf30cb6d5502e9f65dcc3c"
  # ipkg   = "toml.ipkg"
  """

covering
main : IO ()
main = run $ do
  dir     <- packDir
  [_,db'] <- getArgs | as => throwE (InvalidArgs as)
  scheme  <- fromMaybe "scheme" <$> getEnv "SCHEME"

  let db   = MkDBName db'
      conf = microInit dir scheme db

  -- initialize `$HOME/.pack/user/pack.toml`
  write (packToml dir) (packToml scheme db)

  updateDB conf
  ignore $ idrisEnv conf
