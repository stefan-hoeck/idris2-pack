module Pack.Config.TOML

import Data.List1
import Data.String

import Pack.Config.Types
import Pack.Core
import Pack.Database

%default total

export
FromTOML Codegen where
  fromTOML = tmap Types.fromString

0 CustomDB : Type
CustomDB = SortedMap DBName (SortedMap PkgName $ Package_ MetaCommit)

cstm :  Path Abs -> Value -> Either TOMLErr CustomDB
cstm = sortedMap (sortedMap package)

||| Parse a `UserConfig` from a TOML value.
|||
||| @ dir : Parent directory of the `.toml` file we read. This
|||         is used to turn relative paths in the `.toml` file
|||         (for instance, relative paths pointing to local Idris
|||         projects) into absolute file paths.
export
config : (dir : Path Abs) -> Value -> Either TOMLErr UserConfig
config dir v =
  [| MkConfig (maybeValAt "collection" v)
              (maybeValAt "idris2.scheme" v)
              (maybeValAt "install.safety-prompt" v)
              (maybeValAt "install.with-src" v)
              (maybeValAt "install.with-docs" v)
              (maybeValAt "install.use-katla" v)
              (pure Nothing)
              (maybeValAt "idris2.repl.rlwrap" v)
              (maybeValAt "install.libs" v)
              (maybeValAt "install.apps" v)
              (maybeValAt' (cstm dir) "custom" v)
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

  # Whether to install API docs together with installed
  # libraries.
  # with-docs = false

  # Whether to use katla to generate HTML links to
  # semantically highlighted API sources
  # use-katla = false

  # Whether to prompt the user before building or installing
  # packages or applications with custom build hooks in their
  # `.ipkg` file.
  safety-prompt = true

  # Must-have libraries. These will be installed automatically
  # when using a new package collection.
  # libs       = [ "toml", "elab-util" ]

  # Must-have applications. These will be installed automatically
  # when using a new package collection.
  # apps       = [ "lsp" ]

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
  # package collections. The path to the package's root
  # directory can be absolute or relative. In the latter
  # case, it will be considered to be relative to the
  # parent directory of the `pack.toml` file where it is
  # defined.
  #
  # The path to `.ipkg` files must always relative to the
  # given `path`.
  # [custom.all.chem]
  # type = "local"
  # path = "/data/idris/chem"
  # ipkg = "chem.ipkg"

  # A package on GitHub to be available with all
  # package collections.
  # [custom.all.foo]
  # type = "github"
  # url  = "https://github.com/bar/foo"
  # ipkg = "foo.ipkg"

  # Override library `toml` from package collection `nightly-220503`
  # by using a custom commit hash.
  # [custom.nightly-220503.toml]
  # type   = "github"
  # url    = "https://github.com/cuddlefishie/toml-idr"
  # commit = "eb7a146f565276f82ebf30cb6d5502e9f65dcc3c"
  # ipkg   = "toml.ipkg"
  """
