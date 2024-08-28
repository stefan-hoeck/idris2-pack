module Pack.Admin.Report.Types

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
data TestResult = TestSuccess | TestFailure | Skipped | NoTests

export
Interpolation TestResult where
  interpolate TestSuccess = "success"
  interpolate TestFailure = "failure"
  interpolate Skipped     = "skipped"
  interpolate NoTests     = ""

public export
data Report : Type where
  Success : SafeLib -> TestResult -> Report
  Failure : SafeLib -> List PkgName -> Report
  Error   : PkgName -> PackErr -> Report

export
failingDeps : List Report -> List PkgName
failingDeps rs = nub $ rs >>=
  \case
    Success _ _  => []
    Failure s [] => [name s]
    Failure _ ss => ss
    Error s err  => [s]

record RepLines where
  constructor MkRL
  errs      : SnocList (PkgName, PackErr)
  failures  : SnocList (SafeLib, List PkgName)
  successes : SnocList (SafeLib, TestResult)

Semigroup RepLines where
  MkRL e1 f1 s1 <+> MkRL e2 f2 s2 = MkRL (e1<+>e2) (f1<+>f2) (s1<+>s2)

Monoid RepLines where
  neutral = MkRL Lin Lin Lin

ghCommitLink : URL -> Commit -> String
ghCommitLink u c@(MkCommit commit)  =
  let shortSha = substr 0 7 commit in
 "[\{shortSha}](\{u}/commit/\{c})"

apiLink : PkgName -> String
apiLink p =
  "https://stefan-hoeck.github.io/idris2-pack-db/docs/\{p}/docs/index.html"

url : (e : Env) => Package -> URL
url (Git u _ _ _ _)            = u
url (Local dir ipkg pkgPath _) = MkURL "\{dir}"
url (Core _)                   = e.db.idrisURL

commit : (e : Env) => Package -> Commit
commit (Git _ c _ _ _)            = c
commit (Local dir ipkg pkgPath _) = ""
commit (Core _)                   = e.db.idrisCommit

succLine : Env => (SafeLib, TestResult) -> String
succLine (lib,tst) =
  let desc := desc lib
      pkg  := pkg lib
      brf  := fromMaybe "" desc.desc.brief
      lic  := fromMaybe "" desc.desc.license
      nm   := name lib
      api  := apiLink nm
      url  := url pkg
      lnk  := ghCommitLink url (commit pkg)
   in "| [\{nm}](\{url}) | \{brf} | \{lic} | \{lnk} | \{tst} | [docs](\{api}) |"

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

      | Package | Description | License | Commit | Tests | API Docs |
      | --- | --- | --- | --- | --- | --- |
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
toRepLines (Success x t) =
  MkRL Lin Lin [< (x,t)]

toRepLines (Failure x ds) =
  MkRL Lin [< (x,ds)] Lin

toRepLines (Error x y) =
  MkRL [< (x,y)] Lin Lin

export
printReport : Env => Foldable t => t Report -> String
printReport = report . foldMap toRepLines

export
numberOfFailures : Foldable t => t Report -> Nat
numberOfFailures = foldl count 0

  where
    count : Nat -> Report -> Nat
    count k (Success _ _) = k
    count k (Failure _ _) = S k
    count k (Error _ _)   = S k

testInfo : TestResult -> String
testInfo TestSuccess = "all tests passed"
testInfo TestFailure = "some tests failed"
testInfo Skipped     = "tests skipped"
testInfo NoTests     = "no tests run"

export
successLine : Report -> Maybe String
successLine (Success x y)  = Just "\{name x} (\{testInfo y})"
successLine _              = Nothing

depsInfo : List PkgName -> String
depsInfo [] = ""
depsInfo xs =
     "(failing dependencies: "
  ++ concat (intersperse ", " $ map value xs)
  ++ ")"

export
failureLine : Report -> Maybe String
failureLine (Failure x xs) = Just "\{name x}\{depsInfo xs}"
failureLine _              = Nothing

export
resolveLine : Report -> Maybe String
resolveLine (Error x y) = Just "\{x}"
resolveLine _           = Nothing
