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

export
FromTOML Codegen where
  fromTOML = tmap Types.fromString

||| Adj configuration.
export
config : Value -> Either TOMLErr (Config_ MetaCommit Maybe Nothing)
config v =
  [| MkConfig (pure Nothing)
              (maybeValAt "collection" v)
              (maybeValAt "idris2.scheme" v)
              (maybeValAt "idris2.bootstrap" v)
              (maybeValAt "install.safety-prompt" v)
              (maybeValAt "install.with-src" v)
              (pure Nothing)
              (maybeValAt "idris2.repl.rlwrap" v)
              (maybeValAt "install.libs" v)
              (maybeValAt "install.apps" v)
              (maybeValAt "custom" v)
              (pure Nothing)
              (pure Nothing)
              (maybeValAt "idris2.codegen" v)
              (pure Nothing)
  |]

export
initToml : (scheme : String) -> (db : DBName) -> String
initToml scheme db = """
  # The package collection to use
  collection = "\{db}"

  [install]

  # Whether to install packages together with their
  # sources or not. This is mainly useful for programmers
  # who have set their editor up with some *go to definition*
  # functionality (for instance by using idris2-lsp with neovim).
  with-src   = true

  # Whether to prompt the user before building or installing
  # packages or applications with custom build hooks in their
  # `.ipkg` file.
  safety-prompt = true

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
  scheme      = "\{scheme}"

  # Default code generator to us
  # codegen     = "chez"

  # Set this to `true` in order to run REPL sessions from within
  # `rlwrap`. This will give you additional features such as a
  # command history.
  repl.rlwrap = false

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
