module Chem.Elem

import public Chem.Types

%default total

public export
data Elem =
    H                                                                                  | He
  | Li | Be                                                   | B  | C  | N  | O  | F  | Ne
  | Na | Mg                                                   | Al | Si | P  | S  | Cl | Ar
  | K  | Ca | Sc | Ti | V  | Cr | Mn | Fe | Co | Ni | Cu | Zn | Ga | Ge | As | Se | Br | Kr
  | Rb | Sr | Y  | Zr | Nb | Mo | Tc | Ru | Rh | Pd | Ag | Cd | In | Sn | Sb | Te | I  | Xe
  | Cs | Ba | La
            | Ce | Pr | Nd | Pm | Sm | Eu | Gd
            | Tb | Dy | Ho | Er | Tm | Yb | Lu
                 | Hf | Ta | W  | Re | Os | Ir | Pt | Au | Hg | Tl | Pb | Bi | Po | At | Rn
  | Fr | Ra | Ac
            | Th | Pa | U  | Np | Pu | Am | Cm
            | Bk | Cf | Es | Fm | Md | No | Lr
                 | Rf | Db | Sg | Bh | Hs | Mt | Ds | Rg | Cn | Nh | Fl | Mc | Lv | Ts | Og

--------------------------------------------------------------------------------
--          Conversion from and to AtomicNr
--------------------------------------------------------------------------------

public export
atomicNr : Elem -> AtomicNr
atomicNr C   = 6
atomicNr O   = 8
atomicNr N   = 7
atomicNr H   = 1
atomicNr He  = 2
atomicNr Li  = 3
atomicNr Be  = 4
atomicNr B   = 5
atomicNr F   = 9
atomicNr Ne  = 10
atomicNr Na  = 11
atomicNr Mg  = 12
atomicNr Al  = 13
atomicNr Si  = 14
atomicNr P   = 15
atomicNr S   = 16
atomicNr Cl  = 17
atomicNr Ar  = 18
atomicNr K   = 19
atomicNr Ca  = 20
atomicNr Sc  = 21
atomicNr Ti  = 22
atomicNr V   = 23
atomicNr Cr  = 24
atomicNr Mn  = 25
atomicNr Fe  = 26
atomicNr Co  = 27
atomicNr Ni  = 28
atomicNr Cu  = 29
atomicNr Zn  = 30
atomicNr Ga  = 31
atomicNr Ge  = 32
atomicNr As  = 33
atomicNr Se  = 34
atomicNr Br  = 35
atomicNr Kr  = 36
atomicNr Rb  = 37
atomicNr Sr  = 38
atomicNr Y   = 39
atomicNr Zr  = 40
atomicNr Nb  = 41
atomicNr Mo  = 42
atomicNr Tc  = 43
atomicNr Ru  = 44
atomicNr Rh  = 45
atomicNr Pd  = 46
atomicNr Ag  = 47
atomicNr Cd  = 48
atomicNr In  = 49
atomicNr Sn  = 50
atomicNr Sb  = 51
atomicNr Te  = 52
atomicNr I   = 53
atomicNr Xe  = 54
atomicNr Cs  = 55
atomicNr Ba  = 56
atomicNr La  = 57
atomicNr Ce  = 58
atomicNr Pr  = 59
atomicNr Nd  = 60
atomicNr Pm  = 61
atomicNr Sm  = 62
atomicNr Eu  = 63
atomicNr Gd  = 64
atomicNr Tb  = 65
atomicNr Dy  = 66
atomicNr Ho  = 67
atomicNr Er  = 68
atomicNr Tm  = 69
atomicNr Yb  = 70
atomicNr Lu  = 71
atomicNr Hf  = 72
atomicNr Ta  = 73
atomicNr W   = 74
atomicNr Re  = 75
atomicNr Os  = 76
atomicNr Ir  = 77
atomicNr Pt  = 78
atomicNr Au  = 79
atomicNr Hg  = 80
atomicNr Tl  = 81
atomicNr Pb  = 82
atomicNr Bi  = 83
atomicNr Po  = 84
atomicNr At  = 85
atomicNr Rn  = 86
atomicNr Fr  = 87
atomicNr Ra  = 88
atomicNr Ac  = 89
atomicNr Th  = 90
atomicNr Pa  = 91
atomicNr U   = 92
atomicNr Np  = 93
atomicNr Pu  = 94
atomicNr Am  = 95
atomicNr Cm  = 96
atomicNr Bk  = 97
atomicNr Cf  = 98
atomicNr Es  = 99
atomicNr Fm  = 100
atomicNr Md  = 101
atomicNr No  = 102
atomicNr Lr  = 103
atomicNr Rf  = 104
atomicNr Db  = 105
atomicNr Sg  = 106
atomicNr Bh  = 107
atomicNr Hs  = 108
atomicNr Mt  = 109
atomicNr Ds  = 110
atomicNr Rg  = 111
atomicNr Cn  = 112
atomicNr Nh  = 113
atomicNr Fl  = 114
atomicNr Mc  = 115
atomicNr Lv  = 116
atomicNr Ts  = 117
atomicNr Og  = 118

public export
fromAtomicNr : AtomicNr -> Elem
fromAtomicNr 6   = C
fromAtomicNr 8   = O
fromAtomicNr 7   = N
fromAtomicNr 1   = H
fromAtomicNr 2   = He
fromAtomicNr 3   = Li
fromAtomicNr 4   = Be
fromAtomicNr 5   = B
fromAtomicNr 9   = F
fromAtomicNr 10  = Ne
fromAtomicNr 11  = Na
fromAtomicNr 12  = Mg
fromAtomicNr 13  = Al
fromAtomicNr 14  = Si
fromAtomicNr 15  = P
fromAtomicNr 16  = S
fromAtomicNr 17  = Cl
fromAtomicNr 18  = Ar
fromAtomicNr 19  = K
fromAtomicNr 20  = Ca
fromAtomicNr 21  = Sc
fromAtomicNr 22  = Ti
fromAtomicNr 23  = V
fromAtomicNr 24  = Cr
fromAtomicNr 25  = Mn
fromAtomicNr 26  = Fe
fromAtomicNr 27  = Co
fromAtomicNr 28  = Ni
fromAtomicNr 29  = Cu
fromAtomicNr 30  = Zn
fromAtomicNr 31  = Ga
fromAtomicNr 32  = Ge
fromAtomicNr 33  = As
fromAtomicNr 34  = Se
fromAtomicNr 35  = Br
fromAtomicNr 36  = Kr
fromAtomicNr 37  = Rb
fromAtomicNr 38  = Sr
fromAtomicNr 39  = Y
fromAtomicNr 40  = Zr
fromAtomicNr 41  = Nb
fromAtomicNr 42  = Mo
fromAtomicNr 43  = Tc
fromAtomicNr 44  = Ru
fromAtomicNr 45  = Rh
fromAtomicNr 46  = Pd
fromAtomicNr 47  = Ag
fromAtomicNr 48  = Cd
fromAtomicNr 49  = In
fromAtomicNr 50  = Sn
fromAtomicNr 51  = Sb
fromAtomicNr 52  = Te
fromAtomicNr 53  = I
fromAtomicNr 54  = Xe
fromAtomicNr 55  = Cs
fromAtomicNr 56  = Ba
fromAtomicNr 57  = La
fromAtomicNr 58  = Ce
fromAtomicNr 59  = Pr
fromAtomicNr 60  = Nd
fromAtomicNr 61  = Pm
fromAtomicNr 62  = Sm
fromAtomicNr 63  = Eu
fromAtomicNr 64  = Gd
fromAtomicNr 65  = Tb
fromAtomicNr 66  = Dy
fromAtomicNr 67  = Ho
fromAtomicNr 68  = Er
fromAtomicNr 69  = Tm
fromAtomicNr 70  = Yb
fromAtomicNr 71  = Lu
fromAtomicNr 72  = Hf
fromAtomicNr 73  = Ta
fromAtomicNr 74  = W
fromAtomicNr 75  = Re
fromAtomicNr 76  = Os
fromAtomicNr 77  = Ir
fromAtomicNr 78  = Pt
fromAtomicNr 79  = Au
fromAtomicNr 80  = Hg
fromAtomicNr 81  = Tl
fromAtomicNr 82  = Pb
fromAtomicNr 83  = Bi
fromAtomicNr 84  = Po
fromAtomicNr 85  = At
fromAtomicNr 86  = Rn
fromAtomicNr 87  = Fr
fromAtomicNr 88  = Ra
fromAtomicNr 89  = Ac
fromAtomicNr 90  = Th
fromAtomicNr 91  = Pa
fromAtomicNr 92  = U
fromAtomicNr 93  = Np
fromAtomicNr 94  = Pu
fromAtomicNr 95  = Am
fromAtomicNr 96  = Cm
fromAtomicNr 97  = Bk
fromAtomicNr 98  = Cf
fromAtomicNr 99  = Es
fromAtomicNr 100 = Fm
fromAtomicNr 101 = Md
fromAtomicNr 102 = No
fromAtomicNr 103 = Lr
fromAtomicNr 104 = Rf
fromAtomicNr 105 = Db
fromAtomicNr 106 = Sg
fromAtomicNr 107 = Bh
fromAtomicNr 108 = Hs
fromAtomicNr 109 = Mt
fromAtomicNr 110 = Ds
fromAtomicNr 111 = Rg
fromAtomicNr 112 = Cn
fromAtomicNr 113 = Nh
fromAtomicNr 114 = Fl
fromAtomicNr 115 = Mc
fromAtomicNr 116 = Lv
fromAtomicNr 117 = Ts
fromAtomicNr 118 = Og

-- since we are dealing with primitive `Bits8` here,
-- Idris needs our help to figure out that we have reached the end
-- of possible inputs.
fromAtomicNr x   =
  assert_total $
  idris_crash "fromAtomicNr called with invalid AtomicNr: \{show x}"

--------------------------------------------------------------------------------
--          Converting from and to String
--------------------------------------------------------------------------------

public export
fromSymbol : String -> Maybe Elem
fromSymbol "C"   = Just C
fromSymbol "O"   = Just O
fromSymbol "N"   = Just N
fromSymbol "H"   = Just H
fromSymbol "He"  = Just He
fromSymbol "Li"  = Just Li
fromSymbol "Be"  = Just Be
fromSymbol "B"   = Just B
fromSymbol "F"   = Just F
fromSymbol "Ne"  = Just Ne
fromSymbol "Na"  = Just Na
fromSymbol "Mg"  = Just Mg
fromSymbol "Al"  = Just Al
fromSymbol "Si"  = Just Si
fromSymbol "P"   = Just P
fromSymbol "S"   = Just S
fromSymbol "Cl"  = Just Cl
fromSymbol "Ar"  = Just Ar
fromSymbol "K"   = Just K
fromSymbol "Ca"  = Just Ca
fromSymbol "Sc"  = Just Sc
fromSymbol "Ti"  = Just Ti
fromSymbol "V"   = Just V
fromSymbol "Cr"  = Just Cr
fromSymbol "Mn"  = Just Mn
fromSymbol "Fe"  = Just Fe
fromSymbol "Co"  = Just Co
fromSymbol "Ni"  = Just Ni
fromSymbol "Cu"  = Just Cu
fromSymbol "Zn"  = Just Zn
fromSymbol "Ga"  = Just Ga
fromSymbol "Ge"  = Just Ge
fromSymbol "As"  = Just As
fromSymbol "Se"  = Just Se
fromSymbol "Br"  = Just Br
fromSymbol "Kr"  = Just Kr
fromSymbol "Rb"  = Just Rb
fromSymbol "Sr"  = Just Sr
fromSymbol "Y"   = Just Y
fromSymbol "Zr"  = Just Zr
fromSymbol "Nb"  = Just Nb
fromSymbol "Mo"  = Just Mo
fromSymbol "Tc"  = Just Tc
fromSymbol "Ru"  = Just Ru
fromSymbol "Rh"  = Just Rh
fromSymbol "Pd"  = Just Pd
fromSymbol "Ag"  = Just Ag
fromSymbol "Cd"  = Just Cd
fromSymbol "In"  = Just In
fromSymbol "Sn"  = Just Sn
fromSymbol "Sb"  = Just Sb
fromSymbol "Te"  = Just Te
fromSymbol "I"   = Just I
fromSymbol "Xe"  = Just Xe
fromSymbol "Cs"  = Just Cs
fromSymbol "Ba"  = Just Ba
fromSymbol "La"  = Just La
fromSymbol "Ce"  = Just Ce
fromSymbol "Pr"  = Just Pr
fromSymbol "Nd"  = Just Nd
fromSymbol "Pm"  = Just Pm
fromSymbol "Sm"  = Just Sm
fromSymbol "Eu"  = Just Eu
fromSymbol "Gd"  = Just Gd
fromSymbol "Tb"  = Just Tb
fromSymbol "Dy"  = Just Dy
fromSymbol "Ho"  = Just Ho
fromSymbol "Er"  = Just Er
fromSymbol "Tm"  = Just Tm
fromSymbol "Yb"  = Just Yb
fromSymbol "Lu"  = Just Lu
fromSymbol "Hf"  = Just Hf
fromSymbol "Ta"  = Just Ta
fromSymbol "W"   = Just W
fromSymbol "Re"  = Just Re
fromSymbol "Os"  = Just Os
fromSymbol "Ir"  = Just Ir
fromSymbol "Pt"  = Just Pt
fromSymbol "Au"  = Just Au
fromSymbol "Hg"  = Just Hg
fromSymbol "Tl"  = Just Tl
fromSymbol "Pb"  = Just Pb
fromSymbol "Bi"  = Just Bi
fromSymbol "Po"  = Just Po
fromSymbol "At"  = Just At
fromSymbol "Rn"  = Just Rn
fromSymbol "Fr"  = Just Fr
fromSymbol "Ra"  = Just Ra
fromSymbol "Ac"  = Just Ac
fromSymbol "Th"  = Just Th
fromSymbol "Pa"  = Just Pa
fromSymbol "U"   = Just U
fromSymbol "Np"  = Just Np
fromSymbol "Pu"  = Just Pu
fromSymbol "Am"  = Just Am
fromSymbol "Cm"  = Just Cm
fromSymbol "Bk"  = Just Bk
fromSymbol "Cf"  = Just Cf
fromSymbol "Es"  = Just Es
fromSymbol "Fm"  = Just Fm
fromSymbol "Md"  = Just Md
fromSymbol "No"  = Just No
fromSymbol "Lr"  = Just Lr
fromSymbol "Rf"  = Just Rf
fromSymbol "Db"  = Just Db
fromSymbol "Sg"  = Just Sg
fromSymbol "Bh"  = Just Bh
fromSymbol "Hs"  = Just Hs
fromSymbol "Mt"  = Just Mt
fromSymbol "Ds"  = Just Ds
fromSymbol "Rg"  = Just Rg
fromSymbol "Cn"  = Just Cn
fromSymbol "Nh"  = Just Nh
fromSymbol "Fl"  = Just Fl
fromSymbol "Mc"  = Just Mc
fromSymbol "Lv"  = Just Lv
fromSymbol "Ts"  = Just Ts
fromSymbol "Og"  = Just Og
fromSymbol _     = Nothing

public export
symbol : Elem -> String
symbol H   = "H"
symbol He  = "He"
symbol Li  = "Li"
symbol Be  = "Be"
symbol B   = "B"
symbol C   = "C"
symbol N   = "N"
symbol O   = "O"
symbol F   = "F"
symbol Ne  = "Ne"
symbol Na  = "Na"
symbol Mg  = "Mg"
symbol Al  = "Al"
symbol Si  = "Si"
symbol P   = "P"
symbol S   = "S"
symbol Cl  = "Cl"
symbol Ar  = "Ar"
symbol K   = "K"
symbol Ca  = "Ca"
symbol Sc  = "Sc"
symbol Ti  = "Ti"
symbol V   = "V"
symbol Cr  = "Cr"
symbol Mn  = "Mn"
symbol Fe  = "Fe"
symbol Co  = "Co"
symbol Ni  = "Ni"
symbol Cu  = "Cu"
symbol Zn  = "Zn"
symbol Ga  = "Ga"
symbol Ge  = "Ge"
symbol As  = "As"
symbol Se  = "Se"
symbol Br  = "Br"
symbol Kr  = "Kr"
symbol Rb  = "Rb"
symbol Sr  = "Sr"
symbol Y   = "Y"
symbol Zr  = "Zr"
symbol Nb  = "Nb"
symbol Mo  = "Mo"
symbol Tc  = "Tc"
symbol Ru  = "Ru"
symbol Rh  = "Rh"
symbol Pd  = "Pd"
symbol Ag  = "Ag"
symbol Cd  = "Cd"
symbol In  = "In"
symbol Sn  = "Sn"
symbol Sb  = "Sb"
symbol Te  = "Te"
symbol I   = "I"
symbol Xe  = "Xe"
symbol Cs  = "Cs"
symbol Ba  = "Ba"
symbol La  = "La"
symbol Ce  = "Ce"
symbol Pr  = "Pr"
symbol Nd  = "Nd"
symbol Pm  = "Pm"
symbol Sm  = "Sm"
symbol Eu  = "Eu"
symbol Gd  = "Gd"
symbol Tb  = "Tb"
symbol Dy  = "Dy"
symbol Ho  = "Ho"
symbol Er  = "Er"
symbol Tm  = "Tm"
symbol Yb  = "Yb"
symbol Lu  = "Lu"
symbol Hf  = "Hf"
symbol Ta  = "Ta"
symbol W   = "W"
symbol Re  = "Re"
symbol Os  = "Os"
symbol Ir  = "Ir"
symbol Pt  = "Pt"
symbol Au  = "Au"
symbol Hg  = "Hg"
symbol Tl  = "Tl"
symbol Pb  = "Pb"
symbol Bi  = "Bi"
symbol Po  = "Po"
symbol At  = "At"
symbol Rn  = "Rn"
symbol Fr  = "Fr"
symbol Ra  = "Ra"
symbol Ac  = "Ac"
symbol Th  = "Th"
symbol Pa  = "Pa"
symbol U   = "U"
symbol Np  = "Np"
symbol Pu  = "Pu"
symbol Am  = "Am"
symbol Cm  = "Cm"
symbol Bk  = "Bk"
symbol Cf  = "Cf"
symbol Es  = "Es"
symbol Fm  = "Fm"
symbol Md  = "Md"
symbol No  = "No"
symbol Lr  = "Lr"
symbol Rf  = "Rf"
symbol Db  = "Db"
symbol Sg  = "Sg"
symbol Bh  = "Bh"
symbol Hs  = "Hs"
symbol Mt  = "Mt"
symbol Ds  = "Ds"
symbol Rg  = "Rg"
symbol Cn  = "Cn"
symbol Nh  = "Nh"
symbol Fl  = "Fl"
symbol Mc  = "Mc"
symbol Lv  = "Lv"
symbol Ts  = "Ts"
symbol Og  = "Og"

--------------------------------------------------------------------------------
--          Implementations
--------------------------------------------------------------------------------

public export
Eq Elem where
  (==) = (==) `on` atomicNr

public export
Ord Elem where
  compare = compare `on` atomicNr

export %inline
Show Elem where
  show = symbol
