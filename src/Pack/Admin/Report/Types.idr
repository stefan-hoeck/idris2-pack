module Pack.Admin.Report.Types

import Data.Maybe
import Data.SnocList
import Data.SortedMap
import Data.String
import Idris.Package.Types
import Pack.Core.Types
import Pack.Config.Types
import Pack.Database.Types
import Pack.Runner.Database

%default total

--------------------------------------------------------------------------------
--          Report
--------------------------------------------------------------------------------

public export
data Report : Type where
  Success : LibOrApp Safe -> Report
  Failure : LibOrApp Safe -> List PkgName -> Report
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
  failures  : SnocList (LibOrApp Safe, List PkgName)
  successes : SnocList (LibOrApp Safe)

Semigroup RepLines where
  MkRL e1 f1 s1 <+> MkRL e2 f2 s2 = MkRL (e1<+>e2) (f1<+>f2) (s1<+>s2)

Monoid RepLines where
  neutral = MkRL Lin Lin Lin

ghCommitLink : URL -> Commit -> String
ghCommitLink u c@(MkCommit commit)  =
  let shortSha = substr 0 7 commit in
 "[\{shortSha}](\{u}/commit/\{c})"

apiLink : PkgName -> String
apiLink p = "https://stefan-hoeck.github.io/idris2-pack-docs/docs/\{p}/index.html"

url : Env s -> Package -> URL
url e (GitHub u _ _ _) = u
url e (Local dir ipkg pkgPath) = MkURL "\{dir}"
url e (Core _) = e.db.idrisURL

commit : Env s -> Package -> Commit
commit e (GitHub _ c _ _) = c
commit e (Local dir ipkg pkgPath) = ""
commit e (Core _) = e.db.idrisCommit

succLine : Env s -> LibOrApp Safe -> String
succLine e loa =
  let desc := desc loa
      pkg  := pkg loa
      brf  := fromMaybe "" desc.desc.brief
      nm   := name loa
      api  := apiLink nm
      url  := url e pkg
      com  := commit e pkg
   in "| [\{nm}](\{url}) | \{brf} | \{ghCommitLink url com} | [docs](\{api}) |"

failLine : Env s -> (LibOrApp Safe, List PkgName) -> String
failLine e (loa,ps) =
  let pkg  := pkg loa
      url  := url e pkg
      com  := commit e pkg
      deps := fastConcat . intersperse ", " $ map interpolate ps
      nm   := name loa
   in "| [\{nm}](\{url}) | \{deps} | \{ghCommitLink url com} |"

errLine : (PkgName, PackErr) -> String
errLine (p,err) = "| \{p} | \{printErr err} |"

report : Env e -> RepLines -> String
report e (MkRL es fs ss) =
  let succs     = unlines $ map (succLine e) (ss <>> Nil)
      fails     = unlines $ map (failLine e) (fs <>> Nil)
      errs      = unlines $ map errLine (es <>> Nil)
      idrisLink = ghCommitLink e.db.idrisURL e.db.idrisCommit
   in """
      # Package Status

      | Compiler | Version | Commit |
      | --- | --- | --- |
      | [Idris2](\{e.db.idrisURL}) | \{e.db.idrisVersion} | \{idrisLink} |

      ## Building Packages

      | Package | Description | Commit | API Docs |
      | --- | --- | --- | --- |
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
