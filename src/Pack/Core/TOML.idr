module Pack.Core.TOML

import Data.List1
import Data.String
import Idris.Package.Types
import Libraries.Utils.Path
import public Language.TOML
import Pack.Core.IO
import Pack.Core.Types

%default total

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

||| Interface for converting a TOML value to a *pack*
||| data type.
public export
interface FromTOML a where
  ||| Tries to convert a `Value` to a value of type `a`.
  |||
  ||| @ file Path to the file that was read
  ||| @ val  toml value
  fromTOML : (val  : Value) -> Either TOMLErr a

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
FromTOML Path where fromTOML = tmap parse

export
FromTOML DBName where fromTOML = tmap MkDBName

export
FromTOML Bool where
  fromTOML (VBoolean b) = Right b
  fromTOML _            = Left $ WrongType [] "String"

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

-- Try to extract a value from a toml `Value`.
valAt' :  FromTOML a
       => (path : String)
       -> (dflt : Maybe a)
       -> (val  : Value)
       -> Either TOMLErr a
valAt' path dflt = go (forget $ split ('.' ==) path)
  where go : List String -> Value -> Either TOMLErr a
        go []        v          = fromTOML v
        go (x :: xs) (VTable y) = case lookup x y of
          Nothing => case dflt of
            Nothing => Left $ MissingKey [x]
            Just a  => Right a
          Just v2 => prefixKey x $ go xs v2
        go _ _                  = Left $ WrongType [] "Table"

export %inline
valAt : FromTOML a => (path : String) -> (val : Value) -> Either TOMLErr a
valAt path = valAt' path Nothing

export
optValAt :  FromTOML a
         => (path : String)
         -> (dflt : a)
         -> (val  : Value)
         -> Either TOMLErr a
optValAt path = valAt' path . Just

--------------------------------------------------------------------------------
--          Reading a TOML File
--------------------------------------------------------------------------------

export covering
readTOML : HasIO io => (path : Path) -> EitherT PackErr io Value
readTOML path = do
  str <- read path
  case parseTOML str of
    Right v  => pure (VTable v)
    Left err => throwE $ TOMLParse path (show err)

export covering
readFromTOML :  HasIO io
             => (path : Path)
             -> (Value -> Either TOMLErr a)
             ->  EitherT PackErr io a
readFromTOML path f = do
  v <- readTOML path
  liftEither $ mapFst (TOMLFile path) (f v)
