module Pack.CmdLn.Completion

import Data.List
import Data.String
import Libraries.Data.List.Extra
import Libraries.Data.SortedMap
import Pack.CmdLn.Env
import Pack.CmdLn.Types
import Pack.Core
import Pack.Database.Types

--------------------------------------------------------------------------------
--          Bash Autocompletions
--------------------------------------------------------------------------------

-- list of `.ipkg` files in the current directory
ipkgFiles : HasIO io => io (List String)

-- list of package collections in `$HOME/.pack/db`
collections : HasIO io => Env s -> io (List String)

-- list of packages in the currently selected data
-- collection
packages : Env s -> List String
packages e = value <$> keys e.db.packages

-- Packages or `.ipkg` files in the current directory
anyPackage : HasIO io => Env s -> io (List String)
anyPackage e = (++ packages e) <$> ipkgFiles

-- TODO: This should list only installed packages
installedPackages : HasIO io => Env s -> io (List String)
installedPackages = anyPackage

-- keep only those Strings, of which `x` is a prefix
prefixOnly : String -> List String -> List String
prefixOnly x = sortedNub . filter (\s => x /= s && isPrefixOf x s)

-- filter a list of Strings by the given prefix, but only if
-- the prefix is not "--", bash complete's constant for empty input.
prefixOnlyIfNonEmpty : String -> List String -> List String
prefixOnlyIfNonEmpty "--" = id
prefixOnlyIfNonEmpty s    = prefixOnly s

optionFlags : List String

-- given a pair of strings, the first representing the word
-- actually being edited, the second representing the word
-- before the one being edited, return a list of possible
-- completions. If the list of completions is empty, bash
-- will perform directory completion.
opts : HasIO io => Env s -> String -> String -> io (List String)
opts e "--" "pack"  = pure optionFlags

-- options
opts e x "--package-set"    = prefixOnlyIfNonEmpty x <$> collections e
opts e x "-p"               = prefixOnlyIfNonEmpty x <$> collections e

-- actions
opts e x "build"            = prefixOnlyIfNonEmpty x <$> ipkgFiles
opts e x "check-db"         = prefixOnlyIfNonEmpty x <$> collections e
opts e x "exec"             = prefixOnlyIfNonEmpty x <$> anyPackage e
opts e x "install"          = prefixOnlyIfNonEmpty x <$> anyPackage e
opts e x "install-app"      = prefixOnlyIfNonEmpty x <$> anyPackage e
opts e x "install-with-src" = prefixOnlyIfNonEmpty x <$> anyPackage e
opts e x "remove"           = prefixOnlyIfNonEmpty x <$> installedPackages e
opts e x "switch"           = prefixOnlyIfNonEmpty x <$> collections e
opts e x "typecheck"        = prefixOnlyIfNonEmpty x <$> ipkgFiles

-- options
opts e x _ = pure $ if (x `elem` optionFlags)
                      -- `x` is already a known option => perform
                      -- directory completion
                      then Nil
                      else prefixOnly x optionFlags

-- bash autocompletion script using the given function name
completionScript : (fun : String) -> String
completionScript fun = let fun' = "_" ++ fun in """
  \{ fun' }()
  {
    ED=$([ -z $2 ] && echo "--" || echo $2)
    COMPREPLY=($(pack --bash-completion $ED $3))
  }

  complete -F \{ fun' } -o default pack
  """
