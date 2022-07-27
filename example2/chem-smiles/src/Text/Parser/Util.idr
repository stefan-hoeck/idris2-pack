module Text.Parser.Util

import public Data.List.Suffix
import Data.Vect

%default total

--------------------------------------------------------------------------------
--          Parser State
--------------------------------------------------------------------------------

||| Result of a single parsing step.
|||
||| @canFail : Boolean flag indicating, whether this parsing step
|||            can fail or not.
|||
||| @strict  : Boolean flag indicating, whether the parser will
|||            return a strict suffix of the input list
|||
||| @ts      : The list of tokens the parser got as input
|||
||| @err     : Error type in case of a failure.
|||
||| @a       : Result type in case of a success.
public export
data Res :  (canFail : Bool)
         -> (strict  : Bool)
         -> (ts      : List tok)
         -> (err     : Type)
         -> (a       : Type)
         -> Type where
  ||| Failure state
  N :  (ve : err) -> Res True strict ts err a

  ||| Success state
  |||
  ||| This contains an erased proof that the remaining
  ||| list of tokens is a (possibly strict) suffix
  ||| of the input list.
  Y :  (va   : a)
    -> (toks : List t)
    -> (0 prf : Suffix_ strict toks ts)
    -> Res canFail strict ts err a

||| Alias for parsing steps that will always succeed.
||| These must be non-strict, since they will return
||| a suffix for every input list, even the empty one.
public export
0 ResI : List tok -> Type -> Type -> Type
ResI ts err a = Res False False ts err a

||| Alias for non-strict parsing steps that may fail.
public export
0 ResM : List tok -> Type -> Type -> Type
ResM = Res True False

||| Alias for strict parsing steps that may fail.
public export
0 ResS : List tok -> Type -> Type -> Type
ResS = Res True True

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

||| Convenient constructor for those occasions where Idris2
||| can construct a proof that `toks` is a suffix of the input
||| list `ts` on its own, which is typically the case if
||| we just consume part of a list by pattern matching.
public export
y :  (toks : List tok)
  -> {auto 0 prf : Suffix_ strict toks ts}
  -> (va : a)
  -> Res canFail strict ts err a
y toks va = Y va toks prf

||| This is a more efficient version of `mapRes`
||| since we do not need to deal with the failure case.
public export
mapResI : (a -> b) -> ResI ts err a -> ResI ts err b
mapResI f (Y va toks prf) = Y (f va) toks prf

||| Mapping a function over the result type
public export
mapRes : (a -> b) -> Res True strict ts err a -> Res True strict ts err b
mapRes f (Y va toks prf) = Y (f va) toks prf
mapRes _ (N err)         = N err

--------------------------------------------------------------------------------
--          Simple Combinators
--------------------------------------------------------------------------------

||| Tries to split a list of tokens into its head and tail
||| failing with `error` in case of an empty list.
export
uncons : (ts : List tok) -> (error : Lazy err) -> ResS ts err tok
uncons (h :: t) _ = y t h
uncons []       e = N e

||| Tries to split a list at the given position.
||| Returns a shorter prefix if the input is not long enough.
export
splitAt : Nat -> (ts : List tok) -> ResI ts err (List tok)
splitAt (S k) (h :: t) =
  let Y res t' p = splitAt {err} k t
   in y t' (h :: res)
splitAt _     ts       = y ts Nil

||| Strict version of `splitAt`. This will fail with `error`
||| if the input is too short. On the other hand, it returns
||| a `Vect n` instead of a `List` as a proof that the result
||| is of exactly the right length.
export
splitAtStrict :  (n     : Nat)
              -> (ts    : List tok)
              -> (error : Lazy err)
              -> Res True (isSucc n) ts err (Vect n tok)
splitAtStrict 0     ts        e = y ts []
splitAtStrict (S k) (x :: xs) e =
  let Y v t p = splitAtStrict k xs e | N e' => N e'
   in Y (x :: v) t (Cons p)
splitAtStrict (S k) []        e = N e

||| Returns a possibly empty prefix of `ts` util `f` returns `False`.
export
takeWhile : (f : tok -> Bool) -> (ts : List tok) -> ResI ts err (List tok)
takeWhile _ [] = y [] []
takeWhile f (x :: xs) =
  if f x
     then let Y res t p = takeWhile {err} f xs in y t (x :: res)
     else y (x :: xs) []

--------------------------------------------------------------------------------
--          Reading Integers
--------------------------------------------------------------------------------

||| Extract a prefix of digits from a list of characters.
export %inline
digits : (cs : List Char) -> ResI cs err $ List Char
digits = takeWhile isDigit

||| Extract and read all digits from the front of a list of chars.
export
readDigits : (String -> a) -> (cs : List Char) -> ResI cs err a
readDigits f cs = mapResI (f . pack) $ digits cs

||| Like `readDigits` but with the possibility of failure
export
parseDigits : (f : String -> Either err a) -> (cs : List Char) -> ResM cs err a
parseDigits f cs =
  let Y ds t p = digits {err} cs
   in either N (\va => Y va t p) (f $ pack ds)

||| Like `parseDigits` but uses the given default value
||| in case of an empty list of digits.
export
parseDigitsDef :  a
               -> (f : String -> Either err a)
               -> (cs : List Char)
               -> ResM cs err a
parseDigitsDef def f cs = case digits {err} cs of
  Y [] _ _ => y cs def
  Y ds t p => either N (y t) (f $ pack ds)
