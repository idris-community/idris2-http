module Network.HTTP.Encoding

import Data.Bits
import Data.List

utf8_bytelen : Bits8 -> Maybe (Bits8, Nat)
utf8_bytelen x =
  if (x .&. 0b01111111) == x then Just (x, 0) -- ascii
    else if (shiftR x 5) == 0b110   then Just (x .&. 0b0011111, 1)
    else if (shiftR x 4) == 0b1110  then Just (x .&. 0b0001111, 2)
    else if (shiftR x 3) == 0b11110 then Just (x .&. 0b0000111, 3)
    else Nothing

utf8_unmask : Bits8 -> Maybe Bits8
utf8_unmask x = const (x .&. 0b00111111) <$> guard (shiftR x 6 == 0b10)

utf8_pushbits : Integer -> List Bits8 -> Integer
utf8_pushbits acc [] = acc
utf8_pushbits acc (x::xs) = utf8_pushbits ((shiftL acc 6) .|. (cast x)) xs

public export
utf8_pack : List Bits8 -> Maybe String
utf8_pack = go []
  where
    go : List Char -> List Bits8 -> Maybe String
    go acc [] = Just $ pack $ reverse acc
    go acc (x :: xs) = do
      (x, l) <- utf8_bytelen x
      let (y,ys) = splitAt l xs
      guard (length y == l)
      y <- traverse utf8_unmask y
      let c = utf8_pushbits (cast x) y
      go ((cast c) :: acc) ys

utf8_char_bytelen : Integer -> (Nat, Bits8)
utf8_char_bytelen x =
  if x <= 0x7F then (0, 0)
    else if x <= 0x07FF then (1, 0b11000000)
    else if x <= 0xFFFF then (2, 0b11100000)
    else (3, 0b11110000) 

utf8_encode : Integer -> List Bits8
utf8_encode i =
  case utf8_char_bytelen i of
    (0, _) => [ cast i ]
    (n, m) => loop [] i n m where
      loop : List Bits8 -> Integer -> Nat -> Bits8 -> List Bits8
      loop acc i Z mask = (cast i .|. mask) :: acc
      loop acc i (S n) mask =
        let b = cast (i .&. 0b111111) .|. 0b10000000
            i' = shiftR i 6
        in loop (b :: acc) i' n mask

public export
utf8_unpack : String -> List Bits8
utf8_unpack str = unpack str >>= utf8_encode . cast
