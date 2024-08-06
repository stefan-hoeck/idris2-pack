module Pack.CmdLn.Completion

import Control.Monad.Trans
import Data.List
import Data.SortedMap
import Libraries.Data.List.Extra
import Pack.CmdLn
import Pack.Config
import Pack.Core
import Pack.Database
import Pack.Runner.Query
import System.Directory

--------------------------------------------------------------------------------
--          Bash Autocompletions
--------------------------------------------------------------------------------

-- list of `.ipkg` files in the current directory
ipkgFiles : HasIO io => io (List String)
ipkgFiles = do
  Right ss <- runEitherT currentEntries | Left _ => pure []
  pure . map interpolate $ filter isIpkgBody ss

toDBName : Body -> Maybe String
toDBName s = case split s of
  Just (db,"toml") => Just "\{db}"
  _                => Nothing

-- list of package collections in `$HOME/.pack/db`
collections : HasIO io => Env => io (List String)
collections = do
  Right ss <- runEitherT (entries dbDir) | Left _ => pure []
  pure $ mapMaybe toDBName ss

-- list of packages in the currently selected data
-- collection
packages : Env => List String
packages = value <$> keys allPackages

-- list of packages in the currently selected data
-- collection
packagesOrIpkg : HasIO io => Env => io (List String)
packagesOrIpkg = do
  ps <- ipkgFiles
  pure (packages ++ ps)

all : HasIO io => Env => io (List QPkg)
all = do
  ei <- runEitherT $ resolveAll
  pure $ either (const []) snd ei

-- Lists only installed packages
installedLibs : HasIO io => Env => io (List String)
installedLibs = map nameStr . filter installedLib <$> all

-- Lists only installed packages
installedApps : HasIO io => Env => io (List String)
installedApps = map nameStr . filter installedApp <$> all

-- Lists only installed packages
apps : HasIO io => Env => io (List String)
apps = map nameStr . filter isApp <$> all

-- keep only those Strings, of which `x` is a prefix
prefixOnly : String -> List String -> List String
prefixOnly x = sortedNub . filter (\s => x /= s && isPrefixOf x s)

-- filter a list of Strings by the given prefix, but only if
-- the prefix is not "--", bash complete's constant for empty input.
prefixOnlyIfNonEmpty : String -> List String -> List String
prefixOnlyIfNonEmpty "--" = id
prefixOnlyIfNonEmpty s    = prefixOnly s

-- list of package types when creating a new package
packageTypes : List String
packageTypes = map interpolate [PLib, PApp]

packageList : String -> List String -> List String
packageList "--" xs = xs
packageList s    xs = case reverse $ split (',' ==) s of
  h ::: _ => prefixOnly h xs

codegens : List String
codegens =
  [ "chez"
  , "chez-sep"
  , "racket"
  , "gambit"
  , "node"
  , "javascript"
  , "refc"
  , "vmcode-interp"
  ]

commands : List String
commands = map fst namesAndCommands

optionFlags : List String
optionFlags = commands ++ optionNames

queries : Env => List String
queries = ["dep", "module"] ++ packages

-- Given a pair of strings, the first representing the word
-- actually being edited, the second representing the word
-- before the one being edited, return a list of possible
-- completions. If the list of completions is empty, bash
-- will perform directory completion.
opts : HasIO io => Env => String -> String -> io (List String)
opts "--" "pack"  = pure optionFlags

-- options
opts x "--package-set"    = prefixOnlyIfNonEmpty x <$> collections
opts x "--with-ipkg"      = prefixOnlyIfNonEmpty x <$> ipkgFiles
opts x "-p"               = prefixOnlyIfNonEmpty x <$> collections
opts x "-P"               = prefixOnlyIfNonEmpty x <$> pure packages
opts x "--packages"       = prefixOnlyIfNonEmpty x <$> pure packages
opts x "--cg"             = prefixOnlyIfNonEmpty x <$> pure codegens
opts x "--log-level"      = prefixOnlyIfNonEmpty x <$> pure (fst <$> logLevels)

-- actions
opts x "app-path"         = prefixOnlyIfNonEmpty x <$> installedApps
opts x "build"            = prefixOnlyIfNonEmpty x <$> ipkgFiles
opts x "install-deps"     = prefixOnlyIfNonEmpty x <$> ipkgFiles
opts x "query"            = prefixOnlyIfNonEmpty x <$> pure queries
opts x "fuzzy"            = packageList          x <$> installedLibs
opts x "dep"              = prefixOnlyIfNonEmpty x <$> pure packages
opts x "modules"          = prefixOnlyIfNonEmpty x <$> pure packages
opts x "check-db"         = prefixOnlyIfNonEmpty x <$> collections
opts x "run"              = prefixOnlyIfNonEmpty x <$> packagesOrIpkg
opts x "install"          = prefixOnlyIfNonEmpty x <$> pure packages
opts x "test"             = prefixOnlyIfNonEmpty x <$> pure packages
opts x "install-app"      = prefixOnlyIfNonEmpty x <$> apps
opts x "remove"           = prefixOnlyIfNonEmpty x <$> installedLibs
opts x "remove-app"       = prefixOnlyIfNonEmpty x <$> installedApps
opts x "switch"           = prefixOnlyIfNonEmpty x . ("latest" ::)
                            <$> collections
opts x "clean"            = prefixOnlyIfNonEmpty x <$> ipkgFiles
opts x "typecheck"        = prefixOnlyIfNonEmpty x <$> ipkgFiles
opts x "new"              = prefixOnlyIfNonEmpty x <$> pure packageTypes
opts x "uninstall"        = pure Nil
opts x "help"             = prefixOnlyIfNonEmpty x <$> pure commands

-- options
opts x _ = pure $
  if (x `elem` optionFlags)
    -- `x` is already a known option => perform
    -- directory completion
    then Nil
    else prefixOnly x optionFlags

||| Prints tab-completion options based on the last and second-to-last
||| command line argument.
export
complete : HasIO io => String -> String -> Env -> EitherT PackErr io ()
complete a b e = do
  os <- lift $ opts a b
  putStr $ unlines os

||| Bash autocompletion script using the given function name
export
completionScript : (fun : String) -> String
completionScript fun = let fun' = "_" ++ fun in """
  \{ fun' }()
  {
    ED=$([ -z $2 ] && echo "--" || echo $2)
    COMPREPLY=($(pack completion $ED $3))
  }

  complete -F \{ fun' } -o default pack
  """
