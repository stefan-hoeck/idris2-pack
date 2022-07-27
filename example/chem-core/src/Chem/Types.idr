module Chem.Types

import public Data.Prim.Bits8
import public Data.Prim.Bits16

%default total

--------------------------------------------------------------------------------
--          Atomic Number
--------------------------------------------------------------------------------

public export
0 IsAtomicNr : Bits8 -> Type
IsAtomicNr v = (v > 0, v < 119)

public export
record AtomicNr where
  constructor MkAtomicNr
  value : Bits8
  0 prf : IsAtomicNr value

public export %inline
Eq AtomicNr where
  MkAtomicNr x _ == MkAtomicNr y _ = x == y

public export %inline
Ord AtomicNr where
  compare (MkAtomicNr x _) (MkAtomicNr y _) = compare x y

public export %inline
Show AtomicNr where
  show (MkAtomicNr v _) = show v

namespace AtomicNr
  public export %inline
  fromInteger : (v : Integer) -> (0 p : IsAtomicNr $ cast v) => AtomicNr
  fromInteger v = MkAtomicNr (cast v) p

  export
  refine : Bits8 -> Maybe AtomicNr
  refine v = case comp 0 v of
    LT _ _ _ => case comp v 119 of
      LT _ _ _ => Just $ MkAtomicNr v %search
      _        => Nothing
    _        => Nothing

--------------------------------------------------------------------------------
--          Mass Number
--------------------------------------------------------------------------------

public export
0 IsMassNr : Bits16 -> Type
IsMassNr v = (v > 0, v < 512)

public export
record MassNr where
  constructor MkMassNr
  value : Bits16
  0 prf : IsMassNr value

public export %inline
Eq MassNr where
  MkMassNr x _ == MkMassNr y _ = x == y

public export %inline
Ord MassNr where
  compare (MkMassNr x _) (MkMassNr y _) = compare x y

public export %inline
Show MassNr where
  show (MkMassNr v _) = show v

namespace MassNr
  public export %inline
  fromInteger : (v : Integer) -> (0 p : IsMassNr $ cast v) => MassNr
  fromInteger v = MkMassNr (cast v) p

  export
  refine : Bits16 -> Maybe MassNr
  refine v = case comp 0 v of
    LT _ _ _ => case comp v 512 of
      LT _ _ _ => Just $ MkMassNr v %search
      _        => Nothing
    _        => Nothing

--------------------------------------------------------------------------------
--          Tests
--------------------------------------------------------------------------------

carbon : AtomicNr
carbon = 6

carbonMass : MassNr
carbonMass = 12
