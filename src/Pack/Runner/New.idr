module Pack.Runner.New

import Data.List1
import Data.Maybe
import Data.String
import Data.String.Extra
import Idris.Package.Types
import Pack.Config.Env
import Pack.Config.Types
import Pack.Core
import Core.Name.Namespace
import Libraries.Text.PrettyPrint.Prettyprinter
import Libraries.Text.PrettyPrint.Prettyprinter.Doc
import Libraries.Text.PrettyPrint.Prettyprinter.Util
import Libraries.Text.PrettyPrint.Prettyprinter.Render.String

%default total

newPkgDesc : (name : String) -> (mod : String) -> (user: String) -> PkgDesc
newPkgDesc name mod user = let ipkg = initPkgDesc name 
                              in { authors := Just user, 
                                   version := Just (MkPkgVersion (0 ::: [0, 1])),
                                   mainmod := toMaybe (mod == "Main") (nsAsModuleIdent $ mkNamespace mod, ""),
                                   sourcedir := Just "src" } ipkg

-- Helper to capitalize the first letter of a given String
capitalize : String -> String
capitalize s with (strM s)
  capitalize "" | StrNil = ""
  capitalize (strCons x xs) | (StrCons x xs) = strCons (toUpper x) xs

mainModFile : String 
mainModFile = """
              module Main
                 
              main : IO ()
              main = putStrLn "Hello from Idris2!"
 
              """

libModFile : String -> String 
libModFile name = """
                  module \{name}

                  test : String
                  test = "Hello from Idris2!"
      
                  """

||| Create a new package at current location
export covering
new : HasIO io => PkgType -> (pkgName : String) -> Env HasIdris -> EitherT PackErr io ()
new pty pkgName e = do
    debug e "Creating new \{pty} package named \{pkgName}..."
    debug e "Getting author name from git config"
    user <- map trim (sysRun "git config user.name")
    debug e "Creating PkgDesc"
    let (mod, modFile) : (String, String) = case pty of 
                                                 Lib => (capitalize pkgName, libModFile $ capitalize pkgName)
                                                 Bin => ("Main", mainModFile)

    ipkg <- pure $ newPkgDesc pkgName mod user

    debug e "Creating parent and src directories"
    let pkgRootDir = parse "./\{pkgName}"
    let srcDir = pkgRootDir /> "src"
    mkDir (srcDir)

    debug e "Writing ipkg file"
    write (pkgRootDir /> "\{pkgName}.ipkg") (renderString $ layoutUnbounded $ pretty ipkg)

    debug e "Writing module file"
    write (srcDir /> "\{mod}.idr") modFile
    info e "Created \{pty} package '\{pkgName}'"