||| Utilities for parsing .ipkg files. Most of this stuff
||| should actually be available from the Idris API after
||| some small refactoring.
module Pack.Core.Ipkg

import Core.Core
import Core.FC
import Core.Name.Namespace
import Idris.Package.Types
import Libraries.Data.String.Extra
import Libraries.Text.Parser
import Pack.Core.Types
import Pack.Core.IO
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
field fname =
      strField PAuthors "authors"
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
             pure
               [ LT (MkPkgVersion (fromInteger <$> vs)) True
               , GT (MkPkgVersion (fromInteger <$> vs)) True
               ]

    mkBound : List Bound -> PkgVersionBounds -> EmptyRule PkgVersionBounds
    mkBound (LT b i :: bs) pkgbs =
      maybe
        (mkBound bs ({ upperBound := Just b, upperInclusive := i } pkgbs))
        (\_ => fail "Dependency already has an upper bound")
        pkgbs.upperBound
    mkBound (GT b i :: bs) pkgbs =
      maybe
        (mkBound bs ({ lowerBound := Just b, lowerInclusive := i } pkgbs))
        (\_ => fail "Dependency already has a lower bound")
        pkgbs.lowerBound
    mkBound [] pkgbs = pure pkgbs

    langversions : EmptyRule PkgVersionBounds
    langversions = do
      bs <- sepBy andop bound
      mkBound (concat bs) anyBounds

    depends : Rule Depends
    depends = do
      name <- packageName
      bs <- sepBy andop bound
      pure (MkDepends name !(mkBound (concat bs) anyBounds))

    strField : (FC -> String -> DescField) -> String -> Rule DescField
    strField fieldConstructor fieldName = do
      start <- location
      ignore $ exactProperty fieldName
      equals
      str <- stringLit
      end <- location
      pure $ fieldConstructor (MkFC (PhysicalPkgSrc fname) start end) str

pkgDesc : String -> Rule (String, List DescField)
pkgDesc fname = do
  ignore $ exactProperty "package"
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

addFields : (name : String) -> List DescField -> PkgDesc
addFields = foldl addField . initPkgDesc

parseIpkg : File Abs -> String -> Either PackErr PkgDesc
parseIpkg file str =
  let err := InvalidIpkgFile file
   in do
     toks           <- mapFst (const err) $ lex str
     (_, (n,fs), _) <- mapFst (const err) $ parse (pkgDesc "\{file}") toks
     Right $ addFields n fs

export covering
parseIpkgFile :
     {auto _ : HasIO io}
  -> (file   : File Abs)
  -> (tmpLoc : File Abs)
  -> EitherT PackErr io (Desc U)
parseIpkgFile file loc = do
  str  <- read file
  desc <- liftEither (parseIpkg file str)
  pure (MkDesc desc str loc ())

--------------------------------------------------------------------------------
--          Extracting Infos
--------------------------------------------------------------------------------

||| Extract the absolute path to the source directory
||| from a package description plus its file location.
export
sourcePath : Desc t -> Path Abs
sourcePath d =
  maybe
    d.path.parent
    (toAbsPath d.path.parent . fromString)
    d.desc.sourcedir

||| Extract the absolute path to the build directory
||| from a package description plus its file location.
export
buildPath : Desc t -> Path Abs
buildPath d =
  maybe
    (d.path.parent /> "build")
    (toAbsPath d.path.parent . fromString)
    d.desc.builddir

||| Extract the (optional) name of the executable from the
||| description of an Idris app.
export
exec : Desc t -> Maybe Body
exec d = d.desc.executable >>= parse

||| Extract the absolute path to an application's
||| executable in the build directory.
export
execPath : Desc t -> (Maybe $ File Abs)
execPath d = (MkF $ buildPath d /> "exec") <$> exec d

--------------------------------------------------------------------------------
--          Docs
--------------------------------------------------------------------------------

||| Path to different files relevant during generation of API docs
||| of a single Idris source file (field `srcFile`).
public export
record DocSources where
  constructor MkDS
  htmlDoc : File Abs
  srcFile : File Abs
  ttmFile : File Abs
  srcHtml : File Abs

replaceDot : Char -> Char
replaceDot '.' = '/'
replaceDot c   = c

||| Generates the doc paths based on the package description
||| (which might use custom source and build directories).
export
sourceForDoc : TTCVersion => Desc t -> File Abs -> Maybe DocSources
sourceForDoc d f = do
  MkBody cs p <- fileStem f
  rf          <- RelFile.parse . pack $ map replaceDot cs
  Just $
    MkDS
      { htmlDoc = f
      , srcFile = (sourcePath d </> rf) <.> "idr"
      , ttmFile = ttm rf
      , srcHtml = MkF (f.parent) (MkBody cs p <.> "src.html")
      }

  where
    ttm : (rf : File Rel) -> File Abs
    ttm rf = case ttcVersion of
      Just v  => (buildPath d </> "ttc" /> v </> rf) <.> "ttm"
      Nothing => (buildPath d </> "ttc" </> rf) <.> "ttm"

||| Insert a link to the katla-generated and highlighted
||| sources to the API docs.
export covering
insertSources : HasIO io => DocSources -> EitherT PackErr io ()
insertSources x = do
  str <- read x.htmlDoc
  write x.htmlDoc (unlines . map (pack . insertSrc . unpack) $ lines str)
  where
    beforeH1 : List Char -> List Char
    beforeH1 [] = []
    beforeH1 ('<' :: '/' :: 'h' :: '1' :: '>' :: t) =
      unpack "</h1><span style=\"float:right\">" ++
      unpack "(<a href=\"\{x.srcHtml.file}\">source</a>)</span>" ++ t
    beforeH1 (h :: t) = h :: beforeH1 t

    insertSrc : List Char -> List Char
    insertSrc []                              = []
    insertSrc ('<' :: 'h' :: '1' :: '>' :: t) = unpack "<h1>" ++ beforeH1 t
    insertSrc (h :: t)                        = h :: insertSrc t
