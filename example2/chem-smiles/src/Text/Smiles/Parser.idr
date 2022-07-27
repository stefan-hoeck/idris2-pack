module Text.Smiles.Parser

import Chem.Elem
import Data.BitMap
import Data.Maybe
import Data.String
import Text.Parser.Util
import Text.Smiles.Types

%default total

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

public export
data DotOrBond = B Bond | Dot | NoDOB

public export
data Err : Type where
  EndOfInput             : Err
  ExpectedAtom           : Err
  ExpectedClosingBracket : Err
  ExpectedElement        : Err
  UnexpectedCP           : Err
  UnexpectedOP           : Err
  InvalidRingNr          : String -> Err

||| An atom paired with its node index
public export
record BAtom where
  constructor MkBAtom
  node : Node
  atom : OrgSubset

||| An atom with a ring bond
public export
record RAtom where
  constructor MkRAtom
  atom : BAtom
  bond : Maybe Bond

public export
record ST where
  constructor MkST
  node  : Node
  atom  : BAtom
  stack : List BAtom
  rings : BitMap RAtom
  mol   : SmilesMol

public export
data Result : Type where
  End   : (result : Graph Bond OrgSubset) -> Result
  Stuck : Err -> List Char -> Result

--------------------------------------------------------------------------------
--          Updating State
--------------------------------------------------------------------------------

isAromatic : OrgSubset -> Bool
isAromatic (OA _)    = True
isAromatic _         = False

addBond' : Bond -> Node -> Node -> SmilesMol -> SmilesMol
addBond' b n1 n2 mol = case mkEdge n1 n2 of
  Just e  => insEdge (MkLEdge e b) mol
  Nothing => mol

addBond : Maybe Bond -> BAtom -> BAtom -> SmilesMol -> SmilesMol
addBond m (MkBAtom n1 a1) (MkBAtom n2 a2) mol =
  let b = fromMaybe (if isAromatic a1 && isAromatic a2 then Arom else Sngl) m
   in addBond' b n1 n2 mol

addRing : Maybe Bond -> RingNr -> ST -> ST
addRing b1 r (MkST n a1 s rs g) =
  let k = cast {to = Key} r.value
   in case lookup k rs of
        Nothing              => MkST n a1 s (insert k (MkRAtom a1 b1) rs) g
        Just (MkRAtom a2 b2) =>
          MkST n a1 s (delete k rs) $ addBond (b1 <|> b2) a1 a2 g

--------------------------------------------------------------------------------
--          Atoms
--------------------------------------------------------------------------------

digs : (cs : List Char) -> ResI cs Err $ String
digs cs = mapResI pack $ takeWhile isDigit cs

atom : (cs : List Char) -> ResS cs Err OrgSubset
atom ('C'::'l'::t) = y t Cl
atom ('C'     ::t) = y t C
atom ('c'     ::t) = y t (OA CArom)
atom ('N'     ::t) = y t N
atom ('n'     ::t) = y t (OA NArom)
atom ('O'     ::t) = y t O
atom ('o'     ::t) = y t (OA OArom)
atom ('F'     ::t) = y t F
atom ('B'::'r'::t) = y t Br
atom ('S'     ::t) = y t S
atom ('s'     ::t) = y t (OA SArom)
atom ('P'     ::t) = y t P
atom ('p'     ::t) = y t (OA PArom)
atom ('I'     ::t) = y t I
atom ('B'     ::t) = y t B
atom ('b'     ::t) = y t (OA BArom)
atom cs            = N ExpectedAtom

--------------------------------------------------------------------------------
--          Rings and Bonds
--------------------------------------------------------------------------------

bnd : (cs : List Char) -> ResI cs Err (Maybe Bond)
bnd ('='  :: t) = y t (Just Dbl)
bnd ('-'  :: t) = y t (Just Sngl)
bnd (':'  :: t) = y t (Just Arom)
bnd ('#'  :: t) = y t (Just Trpl)
bnd ('$'  :: t) = y t (Just Quad)
bnd ('/'  :: t) = y t Nothing
bnd ('\\' :: t) = y t Nothing
bnd cs         = y cs Nothing

dob : (cs : List Char) -> ResI cs Err DotOrBond
dob ('.' :: t) = y t Dot
dob cs         = mapResI (maybe NoDOB B) $ bnd cs

ringNr : (cs : List Char) -> ResS cs Err RingNr
ringNr ('%'::d1::d2::t) =
  let ds = pack [d1,d2]
   in maybe (N $ InvalidRingNr ("%" ++ ds)) (y t) $ read ds
ringNr (d          ::t) =
  let ds = singleton d
   in maybe (N $ InvalidRingNr ds) (y t) $ read ds
ringNr []               = N $ InvalidRingNr ""

--------------------------------------------------------------------------------
--          Parser
--------------------------------------------------------------------------------

headAlpha : List Char -> Bool
headAlpha (c :: _) = isAlpha c || c == '['
headAlpha _        = False

rngs : (cs : List Char) -> (0 _ : SuffixAcc cs) -> ST -> ResI cs Err ST
rngs cs1 (Access rec) st =
  if headAlpha cs1 then y cs1 st
  else
    let Y b   cs2 p2 = bnd cs1
        Y r   cs3 p3 = ringNr cs2 | N _ => y cs1 st
        Y res cs4 p4 = rngs cs3 (rec cs3 $ p3 ~> p2) (addRing b r st)
     in Y res cs4 $ weaken $ p4 ~> p3 ~> p2

atm' : (cs : List Char) -> DotOrBond -> OrgSubset -> ST -> ResI cs Err ST
atm' cs dob a (MkST n a1 s rs g) =
  let a2 = MkBAtom n a
      g2 = insNode n a g
      g3 = case dob of
             NoDOB => addBond Nothing  a1 a2 g2
             B b   => addBond (Just b) a1 a2 g2
             Dot   => g2
   in rngs cs (ssAcc cs) (MkST (n+1) a2 s rs g3)

atm : (cs : List Char) -> ST -> ResS cs Err ST
atm cs1 st =
  if headAlpha cs1
     then
       let Y a   cs2 p2 = atom cs1 | N err => N err
           Y st2 cs3 p3 = atm' cs2 NoDOB a st
        in Y st2 cs3 $ p3 ~> p2
     else
       let Y b   cs2 p2 = dob cs1
           Y a   cs3 p3 = atom cs2 | N err => N err
           Y st2 cs4 p4 = atm' cs3 b a st
        in Y st2 cs4 $ p4 ~> p3 ~> p2

prs : (cs : List Char) -> (0 _ : SuffixAcc cs) -> (st : ST) -> Result
prs cs (Access rec) st =
  case atm cs st of
    Y st2 cs2 p2 => prs cs2 (rec cs2 p2) st2
    N err => case cs of
      '(' :: t => case atm t $ {stack $= (st.atom ::)} st of
         Y st2 cs2 p2 => prs cs2 (rec cs2 $ Cons p2) st2
         N err2       => Stuck err2 t

      ')' :: t => case st.stack of
        a :: as => prs t (rec t cons1) ({stack := as, atom := a} st)
        Nil     => Stuck UnexpectedCP (')' :: t)

      []       =>
        if null (st.rings) && null (st.stack) then End st.mol
        else Stuck EndOfInput []
      _        => Stuck err cs

export
parse : String -> Result
parse "" = End empty
parse s  = case atom (unpack s) of
  N err    => Stuck err (unpack s)
  Y a cs _ =>
    let Y st cs2 _ = rngs cs (ssAcc cs) (MkST 1 (MkBAtom 0 a) Nil empty (insNode 0 a empty))
     in prs cs2 (ssAcc cs2) st
