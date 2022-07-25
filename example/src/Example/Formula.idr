module Example.Formula

import Data.Maybe.NothingMax
import Data.Nat
import Data.Prim.Bits64

import Generics.Derive

%default total

%language ElabReflection

||| A subset of the chemical elements
public export
data Elem = H | He | Li | Be | B | C | N | O | F | Ne

%runElab derive "Example.Formula.Elem" [Generic,Meta,Eq,Ord,Show]

||| Ordering elements by Hill notation
public export
hill : Elem -> Bits64
hill C  = 0
hill H  = 1
hill B  = 2
hill Be = 3
hill F  = 4
hill He = 5
hill Li = 6
hill N  = 7
hill Ne = 8
hill O  = 9

--------------------------------------------------------------------------------
--          Representation of Molecular Formulae
--------------------------------------------------------------------------------

public export
0 (<): Maybe Bits64 -> Maybe Bits64 -> Type
(<) = LT (<)

public export
0 (<=): Maybe Bits64 -> Maybe Bits64 -> Type
m1 <= m2 = Either (m1 < m2) (m1 === m2)

||| A provably sorted, normalized representation
||| of molecular formulae.
public export
data Repr : (ix : Maybe Bits64) -> Type where
  Nil : Repr Nothing
  (::) :  {0 ix : _}
       -> (p     : (Elem,Nat))
       -> (ps    : Repr ix)
       -> (0 prf : Just (hill $ fst p) < ix)
       => (0 nz  : IsSucc (snd p))
       => Repr (Just $ hill $ fst p)

--------------------------------------------------------------------------------
--          Merging Formulae
--------------------------------------------------------------------------------

||| Result of computing the union of two molecular formulae with
||| elem indices `e1` and `e2`. The result's elem index is equal to either
||| `e1` or `e2`
public export
record MergeRes (h1,h2 : Maybe Bits64) where
  constructor MR
  {0 hx  : Maybe Bits64}
  repr   : Repr hx
  0 prf  : Either (h1 === hx) (h2 === hx)

%inline
prepLT : (p : (Elem,Nat))
       -> MergeRes h1 (Just k2)
       -> (0 prf1 : Just (hill $ fst p) < h1)
       => (0 prf2 : Just (hill $ fst p) < Just k2)
       => (0 nz   : IsSucc (snd p))
       => MergeRes (Just $ hill $ fst p) (Just k2)
prepLT p (MR ps prf) =
  let 0 lt = either (trans_LT_EQ prf1) (trans_LT_EQ prf2) prf
   in MR (p :: ps) (Left Refl)

%inline
prepGT : (p : (Elem,Nat))
       -> MergeRes (Just k1) e2
       -> (0 prf1 : Just (hill $ fst p) < e2)
       => (0 prf2 : Just (hill $ fst p) < Just k1)
       => (0 nz   : IsSucc (snd p))
       => MergeRes (Just k1) (Just $ hill $ fst p)
prepGT p (MR ps prf) =
  let 0 lt = either (trans_LT_EQ prf2) (trans_LT_EQ prf1) prf
   in MR (p :: ps) (Right Refl)

%inline
prepEQ :  {0 x : Maybe Bits64}
       -> (p : (Elem,Nat))
       -> (0 eq  : hill (fst p) === k)
       -> MergeRes h1 h2
       -> (0 prf1 : Just (hill $ fst p) < h1)
       => (0 prf2 : Just k < h2)
       => (0 nz   : IsSucc (snd p))
       => MergeRes (Just $ hill $ fst p) x
prepEQ p eq (MR ps prf) =
  let 0 fstp_lt_m2 = rewrite eq in prf2
      0 lt = either (trans_LT_EQ prf1) (trans_LT_EQ fstp_lt_m2) prf
   in MR (p :: ps) (Left Refl)

0 plusSucc : (m,n : Nat) -> (0 prf : IsSucc m) => IsSucc (m + n)
plusSucc (S k) n = ItIsSucc
plusSucc 0 n impossible

||| Merges to molecular formulae
export
merge : Repr h1 -> Repr h2 -> MergeRes h1 h2
merge (p :: ps) (q :: qs) = case comp (hill $ fst p) (hill $ fst q) of
  LT prf _   _   => prepLT p $ merge ps (q :: qs)
  EQ _   prf _   =>
    let 0 nz := plusSucc (snd p) (snd q)
     in prepEQ (fst p, snd p + snd q) prf $ merge ps qs
  GT _   _   prf => prepGT q $ merge (p :: ps) qs
merge y [] = MR y (Left Refl)
merge [] y = MR y (Right Refl)

export
pairs : Repr h1 -> List (Elem,Nat)
pairs (h :: t) = h :: pairs t
pairs []       = []

--------------------------------------------------------------------------------
--          Comparisons
--------------------------------------------------------------------------------

||| Heterogeneous comparison
public export
hcomp : Repr h1 -> Repr h2 -> Ordering
hcomp (p :: ps) (q :: qs) = case compare (hill $ fst p) (hill $ fst q) of
  EQ => case compare (snd p) (snd q) of
    EQ => hcomp ps qs
    o  => o
  o => o
hcomp []        []        = EQ
hcomp []        _         = LT
hcomp _         []        = GT

||| Heterogeneous equality
public export
heq : Repr h1 -> Repr h2 -> Bool
heq x y = hcomp x y == EQ

||| True if the first formula has at least as many atoms of each element
||| as the second formula.
public export
contains_ : Repr h1 -> Repr h2 -> Bool
contains_ (p :: ps) (q :: qs) = case compare (hill $ fst p) (hill $ fst q) of
  LT => contains_ ps (q :: qs)
  EQ => snd p >= snd q && contains_ ps qs
  GT => False
contains_ _  [] = True
contains_ [] _  = False

--------------------------------------------------------------------------------
--          Formula
--------------------------------------------------------------------------------

public export
record Formula where
  constructor FO
  {0 mx : Maybe Bits64}
  pairs : Repr mx

public export %inline
Eq Formula where
  FO x == FO y = heq x y

public export %inline
Ord Formula where
  compare (FO x) (FO y) = hcomp x y

showPair : (Elem,Nat) -> String
showPair (e, 1) = show e
showPair (e, n) = show e ++ show n

export
Show Formula where
  show (FO ps) = fastConcat $ map showPair (pairs ps)

export %inline
Interpolation Formula where
  interpolate = show

export %inline
Semigroup Formula where
  FO x <+> FO y = FO (repr $ merge x y)

export %inline
Monoid Formula where
  neutral = FO []

export %inline
singleton : Elem -> (n : Nat) -> (0 prf : IsSucc n) => Formula
singleton e n = FO [(e,n)]

export %inline
contains : Formula -> Formula -> Bool
contains (FO x) (FO y) = contains_ x y

--------------------------------------------------------------------------------
--          Parsing Formulae
--------------------------------------------------------------------------------

parseSingle : List Char -> Maybe (Elem,List Char)
parseSingle ('H' :: 'e' :: t) = Just (He, t)
parseSingle ('N' :: 'e' :: t) = Just (Ne, t)
parseSingle ('B' :: 'e' :: t) = Just (Be, t)
parseSingle ('L' :: 'i' :: t) = Just (Li, t)
parseSingle ('H' ::  t)       = Just (H, t)
parseSingle ('N' ::  t)       = Just (N, t)
parseSingle ('B' ::  t)       = Just (B, t)
parseSingle ('O' ::  t)       = Just (O, t)
parseSingle ('F' ::  t)       = Just (F, t)
parseSingle _                 = Nothing

parseNat : List Char -> Maybe (Nat,List Char)
