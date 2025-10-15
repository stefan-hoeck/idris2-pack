module Pack.Core.Hash

import Data.Buffer
import Data.Buffer.Core
import Data.Buffer.Indexed
import Text.ParseError

%default total

FNV_OFFSET_BASIS, FNV_PRIME, MASK : Integer
FNV_OFFSET_BASIS = 0x6c62272e07bb014262b821756295c58d
FNV_PRIME        = 0x0000000001000000000000000000013b
MASK             = prim__shl_Integer 1 128 - 1

%inline
xor, and, shr : Integer -> Integer -> Integer
xor = prim__xor_Integer
and = prim__and_Integer
shr = prim__shr_Integer

%inline
mult : Integer -> Integer
mult i = prim__and_Integer MASK (i * FNV_PRIME)

run : (k : Nat) -> (0 p : LTE k n) => IBuffer n -> Integer -> Integer
run 0     b i = i
run (S k) b i = run k b (mult $ xor i (cast $ atNat b k))

toHex : Integer -> String
toHex 0 = "0"
toHex n = go [] (abs n)
  where
    go : List Char -> Integer -> String
    go cs 0 = pack cs
    go cs n = go (hexChar (cast $ and n 0xf)::cs) (assert_smaller n $ shr n 4)

hashStr : Integer -> String -> Integer
hashStr i s = run (cast $ stringByteLength s) (fromString s) i

||| Converts a list of strings to a 128-bit hash, encoded as
||| a hexadecimal string.
|||
||| Uses the [FHV-1a hashing algorithm](https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function)
export
hashStrings : List String -> String
hashStrings = toHex . foldl hashStr FNV_PRIME
