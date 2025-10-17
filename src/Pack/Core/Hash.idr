module Pack.Core.Hash

import Data.Array
import Data.Array.Core as AC
import Data.Array.Mutable
import Data.Bits
import Data.Buffer
import Data.Buffer.Core as BC
import Data.Buffer.Indexed
import Data.ByteString
import Data.ByteVect
import Data.Vect
import Text.ParseError
import Syntax.T1

%hide Data.Linear.(.)
%default total

--------------------------------------------------------------------------------
-- Utilities and Mixing
--------------------------------------------------------------------------------

%foreign "scheme:(lambda (buf o) (bytevector-u64-ref buf o 'little))"
prim__getBits64 : Buffer -> (offset : Integer) -> Bits64

%inline
o64 : Fin 16 -> Nat -> Integer
o64 x o = cast o + 8 * cast x

%inline
shr, shl, xor, or : Bits64 -> Bits64 -> Bits64
shr = prim__shr_Bits64
shl = prim__shl_Bits64
xor = prim__xor_Bits64
or  = prim__or_Bits64

rotr : Bits64 -> Bits64 -> Bits64
rotr w c = (w `shr` c) `or` (w `shl` (64-c))

parameters (v : MArray t n Bits64)

  %inline
  add : (x,y : Fin n) -> F1' t
  add x y = T1.do
    vx <- get v x
    vy <- get v y
    set v x (vx+vy)

  %inline
  addI : (x,y : Fin n) -> Bits64 -> F1' t
  addI x y i = T1.do
    vx <- get v x
    vy <- get v y
    set v x (vx+vy+i)

  %inline
  xorrot : (x,y : Fin n) -> Bits64 -> F1' t
  xorrot x y r = T1.do
    vx <- get v x
    vy <- get v y
    set v x (rotr (xor vx vy) r)

  %inline
  xorat : (x : Fin n) -> Bits64 -> F1' t
  xorat x r = T1.do
    vx <- get v x
    set v x (xor vx r)

  mix : (a,b,c,d : Fin n) -> (x,y : Bits64) -> F1' t
  mix a b c d x y = T1.do
    addI a b x    -- Va ← Va + Vb + x
    xorrot d a 32 -- Vd ← (Vd xor Va) rotateright 32
    add c d       -- Vc ← Vc + Vd
    xorrot b c 24 -- Vb ← (Vb xor Vc) rotateright 24
    addI a b y    -- Va ← Va + Vb + y
    xorrot d a 16 -- Vd ← (Vd xor Va) rotateright 16
    add c d       -- Vc ← Vc + Vd
    xorrot b c 63 -- Vb ← (Vb xor Vc) rotateright 63

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

sigma : IArray 12 (IArray 16 (Fin 16))
sigma =
  array
    [ array [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15]
    , array [14,10, 4, 8, 9,15,13, 6, 1,12, 0, 2,11, 7, 5, 3]
    , array [11, 8,12, 0, 5, 2,15,13,10,14, 3, 6, 7, 1, 9, 4]
    , array [ 7, 9, 3, 1,13,12,11,14, 2, 6, 5,10, 4, 0,15, 8]
    , array [ 9, 0, 5, 7, 2, 4,10,15,14, 1,11,12, 6, 8, 3,13]
    , array [ 2,12, 6,10, 0,11, 8, 3, 4,13, 7, 5,15,14, 1, 9]
    , array [12, 5, 1,15,14,13, 4,10, 0, 7, 6, 3, 9, 2, 8,11]
    , array [13,11, 7,14,12, 1, 3, 9, 5, 0,15, 4, 8, 6, 2,10]
    , array [ 6,15,14, 9,11, 3, 0, 8,12, 2,13, 7, 1, 4,10, 5]
    , array [10, 2, 8, 4, 7, 6, 1, 5,15,11, 9,14, 3,12,13, 0]
    , array [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15]
    , array [14,10, 4, 8, 9,15,13, 6, 1,12, 0, 2,11, 7, 5, 3]
    ]

IV : IArray 8 Bits64
IV =
  array
    [ 0x6a09e667f3bcc908, 0xbb67ae8584caa73b
    , 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1
    , 0x510e527fade682d1, 0x9b05688c2b3e6c1f
    , 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179
    ]

--------------------------------------------------------------------------------
-- Compression
--------------------------------------------------------------------------------

mixIntoH : IArray 16 Bits64 -> MArray s 8 Bits64 -> F1' s
mixIntoH i m = T1.do
  xorat m 0 ((i `at` 0) `xor` (i `at` 8))
  xorat m 1 ((i `at` 1) `xor` (i `at` 9))
  xorat m 2 ((i `at` 2) `xor` (i `at` 10))
  xorat m 3 ((i `at` 3) `xor` (i `at` 11))
  xorat m 4 ((i `at` 4) `xor` (i `at` 12))
  xorat m 5 ((i `at` 5) `xor` (i `at` 13))
  xorat m 6 ((i `at` 6) `xor` (i `at` 14))
  xorat m 7 ((i `at` 7) `xor` (i `at` 15))


0 lte816 : LTE 8 16
lte816 = LTESucc (LTESucc (LTESucc (LTESucc (LTESucc (LTESucc (LTESucc (LTESucc (LTEZero))))))))

parameters (h     : MArray s 8 Bits64)
           (chunk : ByteVect 128)

  %inline
  initV : F1 s (MArray s 16 Bits64)
  initV = T1.do
    v <- marray1 16 0
    AC.copy  h  0 0 8 v {p1 = refl} {p2 = lte816}
    AC.icopy IV 0 8 8 v {p1 = refl} {p2 = refl}
    pure v

  %inline
  m : Fin 16 -> Bits64
  m x = let BV b o _ := chunk in prim__getBits64 (unsafeGetBuffer b) (o64 x o)

  round : MArray s 16 Bits64 -> IArray 16 (Fin 16) -> F1' s
  round v sig = T1.do
    mix v 0 4 8  12 (m (sig `at` 0)) (m (sig `at` 1))
    mix v 1 5 9  13 (m (sig `at` 2)) (m (sig `at` 3))
    mix v 2 6 10 14 (m (sig `at` 4)) (m (sig `at` 5))
    mix v 3 7 11 15 (m (sig `at` 6)) (m (sig `at` 7))

    mix v 0 5 10 15 (m (sig `at` 8))  (m (sig `at` 9))
    mix v 1 6 11 12 (m (sig `at` 10)) (m (sig `at` 11))
    mix v 2 7 8  13 (m (sig `at` 12)) (m (sig `at` 13))
    mix v 3 4 9  14 (m (sig `at` 14)) (m (sig `at` 15))

  compress : (t : Integer) -> (isLastBlock : Bool) -> F1' s
  compress t isLastBlock = T1.do
    v <- initV
    xorat v 12 (cast t)
    xorat v 13 (cast $ prim__shr_Integer t 64)

    when1 isLastBlock $ T1.do
      xorat v 14 0xffff_ffff_ffff_ffff

    round v (sigma `at` 0)
    round v (sigma `at` 1)
    round v (sigma `at` 2)
    round v (sigma `at` 3)
    round v (sigma `at` 4)
    round v (sigma `at` 5)
    round v (sigma `at` 6)
    round v (sigma `at` 7)
    round v (sigma `at` 8)
    round v (sigma `at` 9)
    round v (sigma `at` 10)
    round v (sigma `at` 11)
    vv <- freeze v
    mixIntoH vv h

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

loop : {k : _} -> MArray s 8 Bits64 -> ByteVect k -> Integer -> F1' s
loop h bv i t =
  case tryLTE {n = k} 128 of
    Just0 p =>
     let (pre,pst) := splitAt 128 bv
         i2        := i+128
         _ # t     := compress h pre i2 False t
      in loop h (assert_smaller bv pst) i2 t
    Nothing0 => case tryLTE {n = 128} k of
      Nothing0 => () # t -- impossible
      Just0 p  =>
       let BV ib o q := bv
           mb # t    := mbuffer1 128 t
           _  # t    := BC.icopy ib o 0 k mb {p1 = q} {p2 = p} t
           bb # t    := BC.freeze mb t
        in compress h (BV bb 0 refl) (i+cast k) True t

littleEndian : Nat -> SnocList Char -> Bits64 -> SnocList Char
littleEndian 0     ss m = ss
littleEndian (S k) ss m =
 let b := cast {to = Bits8} m
     y := hexChar (shiftR b 4)
     x := hexChar (b .&. 0xf)
  in littleEndian k (ss:<y:<x) (shr m 8)

initH : Nat -> F1 s (MArray s 8 Bits64)
initH n = T1.do
  h <- marray1 8 0
  AC.icopy IV 0 0 8 h {p1 = refl} {p2 = refl}
  xorat h 0 (0x01010000 + cast n * 8)
  pure h

export
blake2b : (out : Nat) -> (0 p : LTE out 8) => ByteString -> IArray out Bits64
blake2b out (BS _ bv) =
  run1 $ T1.do
    h <- initH out
    loop h bv 0
    AC.freezeLTE h out

export
encodeLE : {k : _} -> IArray k Bits64 -> String
encodeLE = fastPack . (<>> []) . foldl (littleEndian 8) [<]

export
hashStrings : List String -> String
hashStrings = encodeLE . blake2b 2 . fromString . fastConcat
