module Data.Compress.Utils.Bytes

import Syntax.WithProof
import Data.Bits
import Data.DPair
import Data.Fin
import Data.Fin.Extra
import Data.Nat
import Data.List
import Data.Vect
import Data.Nat.Factor
import Data.Nat.Order.Properties

weakenN' : {m : Nat} -> (0 n : Nat) -> Fin m -> Fin (n + m)
weakenN' n m' = rewrite plusCommutative n m in weakenN n m'

fix_fin : (m : Nat) -> (n : Nat) -> (S m) = n -> S (S (S (S (S (S (S (S (mult m 8)))))))) = mult n 8
fix_fin m n prf = rewrite sym prf in Refl

export
to_le : (FiniteBits a, Cast a Bits8) => {n : _} -> {auto 0 no0 : NonZero n} -> {auto 0 prf : n * 8 = (bitSize {a})} -> a -> Vect n Bits8
to_le x = let (S m) = n; nmeq = Refl in map (go nmeq x) Fin.range
  where
    go : {m : Nat} -> ((S m) = n) -> a -> Fin (S m) -> Bits8
    go nmeq b i = cast $ shiftR b (bitsToIndex $ coerce prf $ coerce (fix_fin m n nmeq) $ weakenN' 7 $ i * 8)

export
from_le : (FiniteBits a, Cast Bits8 a) => {n : _} -> {auto 0 no0 : NonZero n} -> {auto 0 prf : n * 8 = (bitSize {a})} -> Vect n Bits8 -> a
from_le p = let (S m) = n; nmeq = Refl in foldl (.|.) zeroBits $ zipWith (go nmeq) p Fin.range
  where
    go : {m : Nat} -> ((S m) = n) -> Bits8 -> Fin (S m) -> a
    go nmeq b i = shiftL (cast b) (bitsToIndex $ coerce prf $ coerce (fix_fin m n nmeq) $ weakenN' 7 $ i * 8)

export
le_to_integer : Foldable t => t Bits8 -> Integer
le_to_integer = go . toList
  where
  go : List Bits8 -> Integer
  go v = foldr (.|.) 0 $ zipWith shiftL (cast {to=Integer} <$> v) ((*8) <$> [0..(length v)])

export
integer_to_le : (n : Nat) -> Integer -> Vect n Bits8
integer_to_le n v = (cast . shiftR v) <$> (((*8) . finToNat) <$> Fin.range)

export
string_to_ascii : (x : String) -> List Bits8
string_to_ascii = map (cast . ord) . unpack

export
ascii_to_string : List Bits8 -> String
ascii_to_string = pack . map cast
