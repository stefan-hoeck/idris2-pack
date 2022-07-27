module Main

import Chem.Elem
import Chem.Formula
import Data.DPair
import Data.Maybe
import Data.Nat
import Hedgehog

atomicNr : Gen AtomicNr
atomicNr = fromMaybe 12 . refine <$> bits8 (linear 1 118)

elem : Gen Elem
elem = fromAtomicNr <$> atomicNr

pos : Gen (Subset Nat IsSucc)
pos = (\n => Element (S n) ItIsSucc) <$> nat (linear 0 99)

toFormula : List (Elem,Subset Nat IsSucc) -> Formula
toFormula = foldMap (\(e,Element n _) => singleton e n)

formula : Gen Formula
formula = toFormula <$> list (linear 0 15) [| MkPair elem pos |]

prop_roundTrip : Property
prop_roundTrip = property $ do
  f <- forAll formula
  readFormula (show f) === Just f

main : IO ()
main = test . pure $ MkGroup "Chem.Formula" [ ("prop_roundTrip", prop_roundTrip) ]
