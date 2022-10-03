module Pack.Runner.New

import Data.List1
import Data.Maybe
import Data.String
import Data.String.Extra
import Idris.Package.Types
import Pack.Config
import Pack.Core
import Core.Name.Namespace
import Libraries.Text.PrettyPrint.Prettyprinter
import Libraries.Text.PrettyPrint.Prettyprinter.Doc
import Libraries.Text.PrettyPrint.Prettyprinter.Util
import Libraries.Text.PrettyPrint.Prettyprinter.Render.String

%default total

newPkgDesc : (name : Body) -> (mod : Body) -> (user: String) -> PkgDesc
newPkgDesc name mod user = let ipkg = initPkgDesc "\{name}"
                              in { authors := Just user,
                                   version := Just (MkPkgVersion (0 ::: [1, 0])),
                                   mainmod := toMaybe (mod == "Main") (nsAsModuleIdent $ mkNamespace "\{mod}", ""),
                                   executable := toMaybe (mod == "Main") "\{name}",
                                   sourcedir := Just "src" } ipkg

-- Helper to capitalize the first letter of a Body
capitalize : Body -> Body
capitalize (MkBody [] prf) impossible
capitalize (MkBody (x :: xs) prf) = case fromChars (toUpper x :: xs) of
  Just b  => b
  Nothing => MkBody (x :: xs) prf

mainModFile : String
mainModFile = """
              module Main

              main : IO ()
              main = putStrLn "Hello from Idris2!"

              """

libModFile : Body -> String
libModFile name = """
                  module \{name}

                  test : String
                  test = "Hello from Idris2!"

                  """

-- Returns module name and module file
getModFile : PkgType -> Body -> (Body, String)
getModFile Lib pkgName = let mod = capitalize pkgName in (mod, libModFile mod)
getModFile Bin pkgName = ("Main", mainModFile)

gitIgnoreFile : String
gitIgnoreFile = """
                build/
                *.*~
                """

||| Create a new package at current location
export covering
new :  HasIO io
    => (curdir : CurDir)
    -> PkgType
    -> (pkgName : Body)
    -> IdrisEnv
    -> EitherT PackErr io ()
new (CD curdir) pty pkgName e = do
    debug "Creating new \{pty} package named \{pkgName}..."
    debug "Getting author name from git config"
    user <- trim <$> sysRun ["git", "config", "user.name"]
    debug "Creating PkgDesc"
    let (mod, modFile) = getModFile pty pkgName

    ipkg <- pure $ newPkgDesc pkgName mod user

    debug "Creating parent and src directories"
    let pkgRootDir = curdir /> pkgName
    let srcDir = pkgRootDir /> "src"
    mkDir (srcDir)

    debug "Initializing git repo"
    eitherT (\err => warn "Git repo creation failed: \{printErr err}")
            (\_ => write (MkF pkgRootDir ".gitignore") gitIgnoreFile)
            (sysAndLog Info ["git", "init", pkgRootDir])

    debug "Writing ipkg file"
    write (MkF pkgRootDir  (pkgName <+> ".ipkg")) (renderString $ layoutUnbounded $ pretty ipkg)

    debug "Writing module file"
    write (MkF srcDir $ mod <+> ".idr") modFile
    info "Created \{pty} package '\{pkgName}'"
