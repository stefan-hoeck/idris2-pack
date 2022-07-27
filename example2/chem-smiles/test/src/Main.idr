module Main

import Data.Vect
import Hedgehog
import Text.Smiles.Parser
import Text.Smiles.Types

atom : Gen String
atom = element ["C", "N", "O"]

bond : Gen String
bond = element ["", "=", "#", "."]

bondWithAtom : Gen String
bondWithAtom = [| bond ++ atom |]

chain : Gen String
chain = fastConcat <$> [| atom :: list (linear 0 10) bondWithAtom |]

isEnd : Result -> Bool
isEnd (End _)     = True
isEnd (Stuck _ _) = False

prop_chain : Property
prop_chain = property $ do
  c <- forAll chain
  isEnd (parse c) === True

main : IO ()
main = test . pure $ MkGroup "Text.Smiles.Parser" [("prop_chain", prop_chain)]
