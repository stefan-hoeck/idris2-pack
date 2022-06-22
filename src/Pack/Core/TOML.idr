module Pack.Core.TOML

import Data.SortedMap as M
import Idris.Package.Types
import public Language.TOML
import Pack.Core.IO
import Pack.Core.Types

%default total

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

||| Interface for using a *pack* data type as a key
||| in a TOML table.
public export
interface Ord a => TOMLKey a where
  ||| Tries to convert a key to a value of type `a`
  fromKey : (k : String) -> Either TOMLErr a

export
TOMLKey DBName where fromKey = Right . MkDBName

export
TOMLKey PkgName where fromKey = Right . MkPkgName

||| Interface for converting a TOML value to a *pack*
||| data type.
public export
interface FromTOML a where
  ||| Tries to convert a `Value` to a value of type `a`.
  fromTOML : (val  : Value) -> Either TOMLErr a

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

||| Read a value of type `b` by reading a value of
||| type `a`.
export
tmap : FromTOML a => (a -> b) -> Value -> Either TOMLErr b
tmap f = map f . fromTOML

||| Read a value of type `b` by refining a value of
||| type `a`.
export
trefine : FromTOML a => (a -> Either TOMLErr b) -> Value -> Either TOMLErr b
trefine f v = fromTOML v >>= f

||| Try to extract a value from a toml `Value`.
export
valAt' :  (get  : Value -> Either TOMLErr a)
       -> (path : String)
       -> (dflt : Maybe a)
       -> (val  : Value)
       -> Either TOMLErr a
valAt' get path dflt = go (forget $ split ('.' ==) path)
  where go : List String -> Value -> Either TOMLErr a
        go []        v          = get v
        go (x :: xs) (VTable y) = case lookup x y of
          Nothing => case dflt of
            Nothing => Left $ MissingKey [x]
            Just a  => Right a
          Just v2 => prefixKey x $ go xs v2
        go _ _                  = Left $ WrongType [] "Table"

||| Extract and convert a mandatory value from a TOML
||| value. The `path` string can contain several dot-separated
||| key names.
export %inline
valAt : FromTOML a => (path : String) -> (val : Value) -> Either TOMLErr a
valAt path = valAt' fromTOML path Nothing

||| Extract and convert an optional value from a TOML
||| value. The `path` string can contain several dot-separated
||| key names.
export %inline
optValAt :  FromTOML a
         => (path : String)
         -> (dflt : a)
         -> (val  : Value)
         -> Either TOMLErr a
optValAt path = valAt' fromTOML path . Just

||| Extract and convert an optional value from a TOML
||| value. The `path` string can contain several dot-separated
||| key names.
export
maybeValAt' :  (f    : Value -> Either TOMLErr a)
            -> (path : String)
            -> (val  : Value)
            -> Either TOMLErr (Maybe a)
maybeValAt' f path = valAt' (map Just . f) path (Just Nothing)

||| Extract and convert an optional value from a TOML
||| value. The `path` string can contain several dot-separated
||| key names.
export %inline
maybeValAt :  FromTOML a
           => (path : String)
           -> (val  : Value)
           -> Either TOMLErr (Maybe a)
maybeValAt = maybeValAt' fromTOML

--------------------------------------------------------------------------------
--          Implementations
--------------------------------------------------------------------------------

export
FromTOML String where
  fromTOML (VString s) = Right s
  fromTOML _           = Left $ WrongType [] "String"

export
FromTOML PkgName where fromTOML = tmap MkPkgName

export
FromTOML URL where fromTOML = tmap MkURL

export
FromTOML Commit where fromTOML = tmap MkCommit

export
FromTOML FilePath where fromTOML = tmap fromString

toRelPath : FilePath -> Either TOMLErr (Path Rel)
toRelPath (FP $ PRel sx) = Right (PRel sx)
toRelPath (FP $ PAbs _)  = Left (WrongType [] "Relative Path")

export
FromTOML (Path Rel) where fromTOML = trefine toRelPath

export
FromTOML DBName where fromTOML = tmap MkDBName

export
FromTOML Bool where
  fromTOML (VBoolean b) = Right b
  fromTOML _            = Left $ WrongType [] "Bool"

export
FromTOML a => FromTOML (List a) where
  fromTOML (VArray vs) = traverse fromTOML vs
  fromTOML _           = Left $ WrongType [] "Array"

readVersion : String -> Either TOMLErr PkgVersion
readVersion s = case traverse parsePositive $ split ('.' ==) s of
  Just ns => Right $ MkPkgVersion ns
  Nothing => Left $ WrongType [] "Package Version"

export
FromTOML PkgVersion where
  fromTOML = trefine readVersion

keyVal :  TOMLKey k
       => (Value -> Either TOMLErr v)
       -> (String,Value)
       -> Either TOMLErr (k,v)
keyVal f (x,y) = prefixKey x [| MkPair (fromKey x) (f y) |]

export
sortedMap :  TOMLKey k
          => (Path Abs -> Value -> Either TOMLErr v)
          -> Path Abs
          -> Value
          -> Either TOMLErr (SortedMap k v)
sortedMap f dir (VTable m) =
  M.fromList <$> traverse (keyVal $ f dir) (M.toList m)
sortedMap _ _    _         = Left $ WrongType [] "Table"

export
TOMLKey k => FromTOML v => FromTOML (SortedMap k v) where
  fromTOML (VTable m) = M.fromList <$> traverse (keyVal fromTOML) (M.toList m)
  fromTOML _          = Left $ WrongType [] "Table"

||| Read an absolute path from a .toml file that could also be given
||| as a path relative to the .toml file's parent directory. We therefore
||| need the parent directory as an additional input.
export
absPathAt :  (path : String)
          -> (dir  : Path Abs) -- parent directory of the .toml file we read
          -> (val  : Value)
          -> Either TOMLErr (Path Abs)
absPathAt path dir val = toAbsPath dir <$> valAt path val

--------------------------------------------------------------------------------
--          Reading a TOML File
--------------------------------------------------------------------------------

||| Reads a file and converts its content to a TOML value.
export covering
readTOML :  HasIO io
         => (path : Path Abs)
         -> EitherT PackErr io Value
readTOML path = do
  str <- read path
  case parseTOML str of
    Right v  => pure (VTable v)
    Left err => throwE $ TOMLParse path (show err)

||| Reads a file, converts its content to a TOML value, and
||| extracts an Idris value from this.
|||
||| @ dir  : Parent directory of the .toml file we read
|||          This is required to convert relative paths
|||          in the .toml file to absolute ones
||| @ file : Name of the .toml file to read
export covering
readFromTOML :  HasIO io
             => (file : Path Abs)
             -> (Path Abs -> Value -> Either TOMLErr a)
             -> EitherT PackErr io a
readFromTOML file f = case parentDir file of
  Just dir => do
    v <- readTOML file
    liftEither $ mapFst (TOMLFile file) (f dir v)
  Nothing  => throwE (TOMLParse file "no parent directory")

||| Reads a file, converts its content to a TOML value, and
||| extracts an Idris value from this.
export covering
readOptionalFromTOML :  HasIO io
                     => (file : Path Abs)
                     -> (Path Abs -> Value -> Either TOMLErr a)
                     -> EitherT PackErr io a
readOptionalFromTOML file f = case parentDir file of
  Just dir => do
    True <- exists file
      | False => liftEither (mapFst (TOMLFile file) . f dir $ VTable empty)
    readFromTOML file f
  Nothing  => throwE (TOMLParse file "no parent directory")
