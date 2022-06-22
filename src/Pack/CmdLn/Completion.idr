module Pack.CmdLn.Completion

import Control.Monad.Trans
import Data.List
import Data.SortedMap
import Libraries.Data.List.Extra
import Pack.CmdLn.Opts
import Pack.Config.Types
import Pack.Core
import Pack.Database.Types
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
toDBName s = case splitFileName s of
  Just (db,"toml") => Just "\{db}"
  _                => Nothing

-- list of package collections in `$HOME/.pack/db`
collections : HasIO io => Env s -> io (List String)
collections e = do
  Right ss <- runEitherT (entries $ dbDir e) | Left _ => pure []
  pure $ mapMaybe toDBName ss

-- list of packages in the currently selected data
-- collection
packages : Env s -> List String
packages e = value <$> keys (allPackages e)

-- list of packages in the currently selected data
-- collection
packagesOrIpkg : HasIO io => Env s -> io (List String)
packagesOrIpkg e = do
  ps <- ipkgFiles
  pure (packages e ++ ps)

-- Lists only installed packages
installedPackages : HasIO io => Env s -> io (List String)
installedPackages e = map (value . name) <$> installed e

-- keep only those Strings, of which `x` is a prefix
prefixOnly : String -> List String -> List String
prefixOnly x = sortedNub . filter (\s => x /= s && isPrefixOf x s)

-- filter a list of Strings by the given prefix, but only if
-- the prefix is not "--", bash complete's constant for empty input.
prefixOnlyIfNonEmpty : String -> List String -> List String
prefixOnlyIfNonEmpty "--" = id
prefixOnlyIfNonEmpty s    = prefixOnly s

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

optionFlags : List String
optionFlags =
  [ "help"
  , "update-db"
  , "query"
  , "run"
  , "fuzzy"
  , "build"
  , "install-deps"
  , "typecheck"
  , "switch"
  , "install"
  , "package-path"
  , "libs-path"
  , "data-path"
  , "remove"
  , "info"
  , "repl"
  , "install-app"
  , "completion"
  , "completion-script"
  ] ++ optionNames

queries : Env s -> List String
queries e = ["dep", "module"] ++ packages e

||| Given a pair of strings, the first representing the word
||| actually being edited, the second representing the word
||| before the one being edited, return a list of possible
||| completions. If the list of completions is empty, bash
||| will perform directory completion.
opts : HasIO io => String -> String -> Env s -> io (List String)
opts "--" "pack"  e = pure optionFlags

-- options
opts x "--package-set"    e = prefixOnlyIfNonEmpty x <$> collections e
opts x "--with-ipkg"      e = prefixOnlyIfNonEmpty x <$> ipkgFiles
opts x "-p"               e = prefixOnlyIfNonEmpty x <$> collections e
opts x "--cg"             e = prefixOnlyIfNonEmpty x <$> pure codegens

-- actions
opts x "build"            e = prefixOnlyIfNonEmpty x <$> ipkgFiles
opts x "install-deps"     e = prefixOnlyIfNonEmpty x <$> ipkgFiles
opts x "query"            e = prefixOnlyIfNonEmpty x <$> pure (queries e)
opts x "fuzzy"            e = packageList          x <$> installedPackages e
opts x "dep"              e = prefixOnlyIfNonEmpty x <$> pure (packages e)
opts x "modules"          e = prefixOnlyIfNonEmpty x <$> pure (packages e)
opts x "check-db"         e = prefixOnlyIfNonEmpty x <$> collections e
opts x "run"              e = prefixOnlyIfNonEmpty x <$> packagesOrIpkg e
opts x "install"          e = prefixOnlyIfNonEmpty x <$> pure (packages e)
opts x "install-app"      e = prefixOnlyIfNonEmpty x <$> pure (packages e)
opts x "remove"           e = prefixOnlyIfNonEmpty x <$> installedPackages e
opts x "switch"           e = prefixOnlyIfNonEmpty x <$> collections e
opts x "typecheck"        e = prefixOnlyIfNonEmpty x <$> ipkgFiles

-- options
opts x _ e = pure $ if (x `elem` optionFlags)
                      -- `x` is already a known option => perform
                      -- directory completion
                      then Nil
                      else prefixOnly x optionFlags

||| Bash autocompletion script using the given function name
export
complete : HasIO io => String -> String -> Env s -> EitherT PackErr io ()
complete a b e = do
  os <- lift $ opts a b e
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
