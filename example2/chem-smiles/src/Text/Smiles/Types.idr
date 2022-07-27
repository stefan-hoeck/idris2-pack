||| A simplified SMILES data type with no support for
||| bracket atoms.
module Text.Smiles.Types

import Chem.Elem
import Data.Prim.Bits8
import public Data.Graph
import Data.String

%default total

--------------------------------------------------------------------------------
--          Atoms
--------------------------------------------------------------------------------

public export
data SubsetAromatic = BArom | CArom | NArom | OArom | SArom | PArom

public export
Eq SubsetAromatic where
  CArom == CArom = True
  BArom == BArom = True
  NArom == NArom = True
  OArom == OArom = True
  SArom == SArom = True
  PArom == PArom = True
  _     == _     = False


public export
data Aromatic = SA SubsetAromatic | SeArom | AsArom

public export
Eq Aromatic where
  SA x   == SA y   = x == y
  SeArom == SeArom = True
  AsArom == AsArom = True
  _      == _      = False


public export
data OrgSubset = B | C | N | O | F | P | S | Cl | Br | I | OA SubsetAromatic

public export
Eq OrgSubset where
  C    == C    = True
  O    == O    = True
  N    == N    = True
  F    == F    = True
  P    == P    = True
  S    == S    = True
  Cl   == Cl   = True
  Br   == Br   = True
  I    == I    = True
  B    == B    = True
  OA x == OA y = True
  _    == _    = False

--------------------------------------------------------------------------------
--          Bonds
--------------------------------------------------------------------------------

public export
record RingNr where
  constructor MkRingNr
  value : Bits8
  0 prf : value < 100

export
refine : Bits8 -> Maybe RingNr
refine m = case comp m 100 of
  LT _ _ _ => Just $ MkRingNr m %search
  _        => Nothing

export
read : String -> Maybe RingNr
read s = parsePositive s >>= refine

public export
data Bond = Sngl | Arom | Dbl | Trpl | Quad

public export
Eq Bond where
  Sngl == Sngl = True
  Arom == Arom = True
  Dbl  == Dbl  = True
  Trpl == Trpl = True
  Quad == Quad = True
  _    == _    = False


public export
SmilesMol : Type
SmilesMol = Graph Bond OrgSubset
