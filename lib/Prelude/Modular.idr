module Modular
import Prelude.Bits

%default total

abstract
data Modulo : Nat -> Type where
    MkModulo : {n : Nat} -> Bits (log2 (2*n)) -> Modulo (S n)

public
modAdd : Modulo n -> Modulo n -> Modulo n
modAdd (MkModulo x) (MkModulo y) = MkModulo (bitsAdd x y)

public
modSub : Modulo n -> Modulo n -> Modulo n
modSub (MkModulo x) (MkModulo y) = MkModulo (bitsSub x y)

public
modMul : Modulo n -> Modulo n -> Modulo n
modMul (MkModulo x) (MkModulo y) = MkModulo (bitsMul x y)

public
modDiv : Modulo n -> Modulo n -> Modulo n
modDiv (MkModulo x) (MkModulo y) = MkModulo (bitsUDiv x y)

instance Num (Modulo (S n)) where
    (+) = modAdd
    (-) = modSub
    (*) = modMul
    abs = id
    fromInteger x = MkModulo (intToBits x)
