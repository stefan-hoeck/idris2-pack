module Pack.Runner.New

import Data.List1
import Data.Maybe
import Data.String
import Idris.Package.Types
import Pack.Config
import Pack.Core
import Core.Name.Namespace
import Libraries.Text.PrettyPrint.Prettyprinter
import Libraries.Text.PrettyPrint.Prettyprinter.Doc
import Libraries.Text.PrettyPrint.Prettyprinter.Util
import Libraries.Text.PrettyPrint.Prettyprinter.Render.String

%default total

newPkgDesc : (name : Body) -> (mod : Body) -> (user: Maybe String) -> PkgDesc
newPkgDesc name mod user =
  let modName := (nsAsModuleIdent $ mkNamespace "\{mod}", "")
   in { authors := user
      , version := Just (MkPkgVersion (0 ::: [1, 0]))
      , mainmod := toMaybe (mod == "Main") modName
      , executable := toMaybe (mod == "Main") "\{name}"
      , modules := guard (mod /= "Main") *> [modName]
      , sourcedir := Just "src"
      } (initPkgDesc "\{name}")

newTestPkgDesc : (name : Body) -> (user: Maybe String) -> PkgDesc
newTestPkgDesc name user =
   { authors    := user
   , version    := Just (MkPkgVersion (0 ::: [1, 0]))
   , mainmod    := Just (nsAsModuleIdent $ mkNamespace "Main", "")
   , executable := Just "\{name}-test"
   , depends    := [MkDepends "\{name}" anyBounds]
   , sourcedir  := Just "src"
   } (initPkgDesc "\{name}-test")

toModuleName : List Char -> List Char
toModuleName [] = []
toModuleName (h :: t) = toUpper h :: map adjHyphen t

  where
    adjHyphen : Char -> Char
    adjHyphen '-' = '_'
    adjHyphen c   = c

-- Helper to capitalize the first letter of a Body
-- and replace hyphens with underscores
capitalize : Body -> Body
capitalize b@(MkBody xs prf) = fromMaybe b $ fromChars (toModuleName xs)

mainModFile : String
mainModFile =
  """
  module Main

  main : IO ()
  main = putStrLn "Hello from Idris2!"

  """

testFile : String
testFile =
  """
  module Main

  main : IO ()
  main = putStrLn "Test successful!"

  """

libModFile : Body -> String
libModFile name =
  """
  module \{name}

  test : String
  test = "Hello from Idris2!"

  """

packTomlContent : Body -> String
packTomlContent name =
  """
  [custom.all.\{name}]
  type = "local"
  path = "."
  ipkg = "\{name}.ipkg"
  test = "test/test.ipkg"

  [custom.all.\{name}-test]
  type = "local"
  path = "test"
  ipkg = "test.ipkg"
  """

-- Returns module name and module file
getModFile : PkgType -> Body -> (Body, String)
getModFile PLib pkgName = let mod = capitalize pkgName in (mod, libModFile mod)
getModFile PApp pkgName = ("Main", mainModFile)

gitIgnoreFile : String
gitIgnoreFile =
  """
  build/
  *.*~

  """

||| Create a new package at current location
export covering
new :
     {auto _ : HasIO io}
  -> (curdir : CurDir)
  -> PkgType
  -> (pkgName : Body)
  -> IdrisEnv
  -> EitherT PackErr io ()
new (CD curdir) pty pkgName e = do
    debug "Creating new \{pty} package named \{pkgName}..."
    debug "Getting author name from git config"
    user <- Just <$> trim <$> sysRun ["git", "config", "user.name"]
            `catchE` (const $ right Nothing)
    debug "Creating PkgDesc"
    let (mod, modFile) = getModFile pty pkgName

    let pkgRootDir := curdir /> pkgName
        srcDir     := pkgRootDir </> "src"
        testDir    := pkgRootDir </> "test"
        ipkg       := newPkgDesc pkgName mod user
        test       := newTestPkgDesc pkgName user

    debug "Creating parent and src directories"

    mkDir (srcDir)

    debug "Initializing git repo"
    eitherT
      (\err => warn "Git repo creation failed: \{printErr err}")
      (\_ => write (pkgRootDir </> ".gitignore") gitIgnoreFile)
      (sysAndLog Info ["git", "init", pkgRootDir])

    debug "Writing ipkg file"
    write
      (pkgRootDir  /> (pkgName <+> ".ipkg"))
      (renderString (layoutUnbounded $ pretty ipkg) ++ "\n")

    debug "Writing test.ipkg file"
    write
      (testDir  </> "test.ipkg")
      (renderString (layoutUnbounded $ pretty test) ++ "\n")

    debug "Writing test Main.idr file"
    write (testDir  </> "src" </> "Main.idr") testFile

    debug "Writing pack.toml"
    write (MkF pkgRootDir packToml) (packTomlContent pkgName)

    debug "Writing module file"
    write (MkF srcDir $ mod <+> ".idr") modFile
    info "Created \{pty} package '\{pkgName}'"
