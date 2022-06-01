module Pack.Admin.Report.Types

import Data.SnocList
import Data.SortedMap
import Data.String
import Pack.Core.Types
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
  errs      : SnocList String
  failures  : SnocList String
  successes : SnocList String

Semigroup RepLines where
  MkRL e1 f1 s1 <+> MkRL e2 f2 s2 = MkRL (e1<+>e2) (f1<+>f2) (s1<+>s2)

Monoid RepLines where
  neutral = MkRL Lin Lin Lin

report : RepLines -> String
report (MkRL errs fails succs) = """
  Packages failing to resolve:

  \{unlines $ errs <>> Nil}

  Packages failing to build:

  \{unlines $ fails <>> Nil}

  Packages building successfully:

  \{unlines $ succs <>> Nil}
  """

toRepLines : Report -> RepLines
toRepLines (Success x) =
  MkRL Lin Lin (Lin :< "  \{name x}")

toRepLines (Failure x []) =
  MkRL Lin (Lin :< "  \{name x}") Lin

toRepLines (Failure x ds) =
  let fl   = "  \{name x}"
      deps = concat $ intersperse ", " $ map value ds
      sl   = "  failing dependencies: \{deps}"
   in MkRL Lin [< fl,sl] Lin
toRepLines (Error x y) =
  MkRL [< "  \{x}: \{printErr y}"] Lin Lin


export
printReport : ReportDB -> String
printReport = report . foldMap toRepLines
