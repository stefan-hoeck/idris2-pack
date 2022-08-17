module Pack.Admin.Report.Types

import Data.Maybe
import Data.SnocList
import Data.SortedMap
import Data.String
import Idris.Package.Types
import Pack.Core
import Pack.Config
import Pack.Database
import Pack.Runner.Database

%default total

--------------------------------------------------------------------------------
--          Report
--------------------------------------------------------------------------------

public export
data Report : Type where
  Success : SafeLib -> Report
  Failure : SafeLib -> List PkgName -> Report
  Error   : PkgName -> PackErr -> Report

public export
0 ReportDB : Type
ReportDB = SortedMap PkgName Report

export
failingDeps : List Report -> List PkgName
failingDeps rs = nub $ rs >>=
  \case Success _    => []
        Failure s [] => [name s]
        Failure _ ss => ss
        Error s err  => [s]

record RepLines where
  constructor MkRL
  errs      : SnocList (PkgName, PackErr)
  failures  : SnocList (SafeLib, List PkgName)
  successes : SnocList (SafeLib)

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

url : (e : Env) => Package -> URL
url (GitHub u _ _ _) = u
url (Local dir ipkg pkgPath) = MkURL "\{dir}"
url (Core _) = e.db.idrisURL

commit : (e : Env) => Package -> Commit
commit (GitHub _ c _ _) = c
commit (Local dir ipkg pkgPath) = ""
commit (Core _) = e.db.idrisCommit

succLine : Env => SafeLib -> String
succLine lib =
  let desc := desc lib
      pkg  := pkg lib
      brf  := fromMaybe "" desc.desc.brief
      nm   := name lib
      api  := apiLink nm
      url  := url pkg
      com  := commit pkg
   in "| [\{nm}](\{url}) | \{brf} | \{ghCommitLink url com} | [docs](\{api}) |"

failLine : Env => (SafeLib, List PkgName) -> String
failLine (lib,ps) =
  let pkg  := pkg lib
      url  := url pkg
      com  := commit pkg
      deps := fastConcat . intersperse ", " $ map interpolate ps
      nm   := name lib
   in "| [\{nm}](\{url}) | \{deps} | \{ghCommitLink url com} |"

errLine : (PkgName, PackErr) -> String
errLine (p,err) = "| \{p} | \{printErr err} |"

report : (e : Env) => RepLines -> String
report (MkRL es fs ss) =
  let succs     = unlines $ map succLine (ss <>> Nil)
      fails     = unlines $ map failLine (fs <>> Nil)
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
printReport : Env => ReportDB -> String
printReport = report . foldMap toRepLines

export
numberOfFailures : ReportDB -> Nat
numberOfFailures = foldl count 0
  where count : Nat -> Report -> Nat
        count k (Success _)   = k
        count k (Failure _ _) = S k
        count k (Error _ _)   = S k
