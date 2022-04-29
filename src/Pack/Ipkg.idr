||| Utilities for parsing .ipkg files. Most of this stuff
||| should actually be available from the Idris API after
||| some small refactoring.
module Pack.Ipkg

import Core.Core
import Core.FC
import Core.Name.Namespace
import Idris.Package.Types
import Libraries.Data.String.Extra
import Libraries.Text.Parser
import Pack.Err
import Pack.Types
import Pack.Util
import Parser.Package
import System.File

%default total

--------------------------------------------------------------------------------
--          Parsing Idris Packages
--------------------------------------------------------------------------------

data DescField  : Type where
  PVersion      : FC -> PkgVersion -> DescField
  PLangVersions : FC -> PkgVersionBounds -> DescField
  PVersionDep   : FC -> String -> DescField
  PAuthors      : FC -> String -> DescField
  PMaintainers  : FC -> String -> DescField
  PLicense      : FC -> String -> DescField
  PBrief        : FC -> String -> DescField
  PReadMe       : FC -> String -> DescField
  PHomePage     : FC -> String -> DescField
  PSourceLoc    : FC -> String -> DescField
  PBugTracker   : FC -> String -> DescField
  PDepends      : List Depends -> DescField
  PModules      : List (FC, ModuleIdent) -> DescField
  PMainMod      : FC -> ModuleIdent -> DescField
  PExec         : String -> DescField
  POpts         : FC -> String -> DescField
  PSourceDir    : FC -> String -> DescField
  PBuildDir     : FC -> String -> DescField
  POutputDir    : FC -> String -> DescField
  PPrebuild     : FC -> String -> DescField
  PPostbuild    : FC -> String -> DescField
  PPreinstall   : FC -> String -> DescField
  PPostinstall  : FC -> String -> DescField
  PPreclean     : FC -> String -> DescField
  PPostclean    : FC -> String -> DescField

field : String -> Rule DescField
field fname
      = strField PAuthors "authors"
    <|> strField PMaintainers "maintainers"
    <|> strField PLicense "license"
    <|> strField PBrief "brief"
    <|> strField PReadMe "readme"
    <|> strField PHomePage "homepage"
    <|> strField PSourceLoc "sourceloc"
    <|> strField PBugTracker "bugtracker"
    <|> strField POpts "options"
    <|> strField POpts "opts"
    <|> strField PSourceDir "sourcedir"
    <|> strField PBuildDir "builddir"
    <|> strField POutputDir "outputdir"
    <|> strField PPrebuild "prebuild"
    <|> strField PPostbuild "postbuild"
    <|> strField PPreinstall "preinstall"
    <|> strField PPostinstall "postinstall"
    <|> strField PPreclean "preclean"
    <|> strField PPostclean "postclean"
    <|> do start <- location
           ignore $ exactProperty "version"
           equals
           vs <- sepBy1 dot' integerLit
           end <- location
           pure (PVersion (MkFC (PhysicalPkgSrc fname) start end)
                          (MkPkgVersion (fromInteger <$> vs)))
    <|> do start <- location
           ignore $ exactProperty "langversion"
           lvs <- langversions
           end <- location
           pure (PLangVersions (MkFC (PhysicalPkgSrc fname) start end) lvs)
    <|> do start <- location
           ignore $ exactProperty "version"
           equals
           v <- stringLit
           end <- location
           pure (PVersionDep (MkFC (PhysicalPkgSrc fname) start end) v)
    <|> do ignore $ exactProperty "depends"
           equals
           ds <- sep depends
           pure (PDepends ds)
    <|> do ignore $ exactProperty "modules"
           equals
           ms <- sep (do start <- location
                         m <- moduleIdent
                         end <- location
                         pure (MkFC (PhysicalPkgSrc fname) start end, m))
           pure (PModules ms)
    <|> do ignore $ exactProperty "main"
           equals
           start <- location
           m <- moduleIdent
           end <- location
           pure (PMainMod (MkFC (PhysicalPkgSrc fname) start end) m)
    <|> do ignore $ exactProperty "executable"
           equals
           e <- (stringLit <|> packageName)
           pure (PExec e)
  where
    data Bound = LT PkgVersion Bool | GT PkgVersion Bool

    bound : Rule (List Bound)
    bound
        = do lte
             vs <- sepBy1 dot' integerLit
             pure [LT (MkPkgVersion (fromInteger <$> vs)) True]
      <|> do gte
             vs <- sepBy1 dot' integerLit
             pure [GT (MkPkgVersion (fromInteger <$> vs)) True]
      <|> do lt
             vs <- sepBy1 dot' integerLit
             pure [LT (MkPkgVersion (fromInteger <$> vs)) False]
      <|> do gt
             vs <- sepBy1 dot' integerLit
             pure [GT (MkPkgVersion (fromInteger <$> vs)) False]
      <|> do eqop
             vs <- sepBy1 dot' integerLit
             pure [LT (MkPkgVersion (fromInteger <$> vs)) True,
                   GT (MkPkgVersion (fromInteger <$> vs)) True]

    mkBound : List Bound -> PkgVersionBounds -> EmptyRule PkgVersionBounds
    mkBound (LT b i :: bs) pkgbs
        = maybe (mkBound bs ({ upperBound := Just b,
                               upperInclusive := i } pkgbs))
                (\_ => fail "Dependency already has an upper bound")
                pkgbs.upperBound
    mkBound (GT b i :: bs) pkgbs
        = maybe (mkBound bs ({ lowerBound := Just b,
                               lowerInclusive := i } pkgbs))
                (\_ => fail "Dependency already has a lower bound")
                pkgbs.lowerBound
    mkBound [] pkgbs = pure pkgbs

    langversions : EmptyRule PkgVersionBounds
    langversions
        = do bs <- sepBy andop bound
             mkBound (concat bs) anyBounds

    depends : Rule Depends
    depends
        = do name <- packageName
             bs <- sepBy andop bound
             pure (MkDepends name !(mkBound (concat bs) anyBounds))

    strField : (FC -> String -> DescField) -> String -> Rule DescField
    strField fieldConstructor fieldName
        = do start <- location
             ignore $ exactProperty fieldName
             equals
             str <- stringLit
             end <- location
             pure $ fieldConstructor (MkFC (PhysicalPkgSrc fname) start end) str

export
parsePkgDesc : String -> Rule (String, List DescField)
parsePkgDesc fname
    = do ignore $ exactProperty "package"
         name <- packageName
         fields <- many (field fname)
         pure (name, fields)

addField : PkgDesc -> DescField -> PkgDesc
addField p (PVersion fc n)       = { version := Just n } p
addField p (PLangVersions fc bs) = { langversion := Just bs } p
addField p (PVersionDep fc n)    = p
addField p (PAuthors fc a)       = { authors := Just a } p
addField p (PMaintainers fc a)   = { maintainers := Just a } p
addField p (PLicense fc a)       = { license := Just a } p
addField p (PBrief fc a)         = { brief := Just a } p
addField p (PReadMe fc a)        = { readme := Just a } p
addField p (PHomePage fc a)      = { homepage := Just a } p
addField p (PSourceLoc fc a)     = { sourceloc := Just a } p
addField p (PBugTracker fc a)    = { bugtracker := Just a } p
addField p (PDepends ds)         = { depends := ds } p
addField p (PModules ms)         = { modules := map (\(_,i) => (i,"")) ms } p
addField p (PMainMod loc n)      = { mainmod := Just (n,"") } p
addField p (PExec e)             = { executable := Just e } p
addField p (POpts fc e)          = { options := Just (fc, e) } p
addField p (PSourceDir fc a)     = { sourcedir := Just a } p
addField p (PBuildDir fc a)      = { builddir := Just a } p
addField p (POutputDir fc a)     = { outputdir := Just a } p
addField p (PPrebuild fc e)      = { prebuild := Just (fc, e) } p
addField p (PPostbuild fc e)     = { postbuild := Just (fc, e) } p
addField p (PPreinstall fc e)    = { preinstall := Just (fc, e) } p
addField p (PPostinstall fc e)   = { postinstall := Just (fc, e) } p
addField p (PPreclean fc e)      = { preclean := Just (fc, e) } p
addField p (PPostclean fc e)     = { postclean := Just (fc, e) } p

export
addFields : (name : String) -> List DescField -> PkgDesc
addFields = foldl addField . initPkgDesc

export
fromLexError : OriginDesc -> (StopReason, Int, Int, String) -> PackErr
fromLexError origin (ComposeNotClosing begin end, _, _, _)
    = LexFail (MkFC origin begin end) "Bracket is not properly closed."
fromLexError origin (_, l, c, _)
    = LexFail (MkFC origin (l, c) (l, c + 1)) "Can't recognise token."

export
fromParsingErrors : Show token => OriginDesc -> List1 (ParsingError token) -> PackErr
fromParsingErrors origin = ParseFail . (map fromError) . forget
  where
    fromError : ParsingError token -> (FC, String)
    fromError (Error msg Nothing)
        = (MkFC origin (0, 0) (0, 0), msg +> '.')
    fromError (Error msg (Just t))
        = let start = startBounds t; end = endBounds t in
            let fc = if start == end
                      then MkFC origin start (mapSnd (+1) start)
                      else MkFC origin start end
            in (fc, msg +> '.')

export
runParser : (fname : String) -> (str : String) -> Rule ty -> Either PackErr ty
runParser fname str p
    = do toks   <- mapFst (\err => fromLexError
                     (PhysicalPkgSrc fname) (NoRuleApply, err)) $ lex str
         (_, val, _) <- mapFst (fromParsingErrors (PhysicalPkgSrc fname)) $ parse p toks
         Right val

export covering
parseFile :  HasIO io
          => (fname : String)
          -> Rule ty
          -> EitherT PackErr io ty
parseFile fname p = MkEitherT $ do
  Right str <- readFile fname
    | Left err => pure (Left (ReadFile fname err))
  pure (runParser fname str p)
