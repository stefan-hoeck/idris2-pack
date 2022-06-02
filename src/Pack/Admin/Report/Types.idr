module Pack.Admin.Report.Types

import Data.Maybe
import Data.SnocList
import Data.SortedMap
import Data.String
import Idris.Package.Types
import Pack.Core.Types
import Pack.Config.Types
import Pack.Database.Types

%default total

--------------------------------------------------------------------------------
--          Report
--------------------------------------------------------------------------------

public export
data Report : Type where
  Success : ResolvedPackage -> Report
  Failure : ResolvedPackage -> List PkgName -> Report
  Error   : PkgName -> PackErr -> Report

public export
0 ReportDB : Type
ReportDB = SortedMap PkgName Report

export
failingDeps : List Report -> List PkgName
failingDeps rs = nub $ rs >>=
  \case Success _    => []
        Failure _ ss => ss
        Error s err  => [s]

record RepLines where
  constructor MkRL
  errs      : SnocList (PkgName, PackErr)
  failures  : SnocList (ResolvedPackage, List PkgName)
  successes : SnocList ResolvedPackage

Semigroup RepLines where
  MkRL e1 f1 s1 <+> MkRL e2 f2 s2 = MkRL (e1<+>e2) (f1<+>f2) (s1<+>s2)

Monoid RepLines where
  neutral = MkRL Lin Lin Lin

succLine : ResolvedPackage -> Maybe String
succLine (RGitHub n u c _ d) =
  let desc = fromMaybe "" d.brief
   in Just "| [\{n}](\{u}) | \{desc} | \{c} |"
succLine _ = Nothing

failLine : (ResolvedPackage, List PkgName) -> Maybe String
failLine (RGitHub n u c _ _, ps) =
  let deps = fastConcat . intersperse ", " $ map interpolate ps
   in Just "| [\{n}](\{u}) | \{deps} | \{c} |"
failLine _ = Nothing

errLine : (PkgName, PackErr) -> String
errLine (p,err) = "| \{p} | \{printErr err} |"

report : Env e -> RepLines -> String
report e (MkRL es fs ss) =
  let succs = unlines $ mapMaybe succLine (ss <>> Nil)
      fails = unlines $ mapMaybe failLine (fs <>> Nil)
      errs  = unlines $ map errLine (es <>> Nil)
   in """
      # Package Status

      | Compiler | Version | Commit |
      | --- | --- | --- |
      | [Idris2](\{e.db.idrisURL}) | \{e.db.idrisVersion} | \{e.db.idrisCommit} |

      ## Building Packages

      | Package | Description | Commit |
      | --- | --- | --- |
      \{succs}

      ## Failing Packages

      | Package | Dependencies | Commit |
      | --- | --- | --- |
      \{fails}

      ## Unresolved Packages

      | Package | Error |
      | --- | --- |
      \{errs}
      """

toRepLines : Report -> RepLines
toRepLines (Success x) =
  MkRL Lin Lin [< x]

toRepLines (Failure x ds) =
  MkRL Lin [< (x,ds)] Lin

toRepLines (Error x y) =
  MkRL [< (x,y)] Lin Lin

export
printReport : Env e -> ReportDB -> String
printReport e = report e . foldMap toRepLines

export
numberOfFailures : ReportDB -> Nat
numberOfFailures = foldl count 0
  where count : Nat -> Report -> Nat
        count k (Success _)   = k
        count k (Failure _ _) = S k
        count k (Error _ _)   = S k
