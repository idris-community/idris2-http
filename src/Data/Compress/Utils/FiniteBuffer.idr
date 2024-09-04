module Data.Compress.Utils.FiniteBuffer

import Data.Seq.Unsized

export
record FiniteBuffer a where
  constructor FB
  max_size : Int
  size : Int
  buffer : Seq a

export
empty : Nat -> FiniteBuffer a
empty n = FB (cast n) 0 empty

export
take_last : Nat -> FiniteBuffer a -> Maybe (List a)
take_last n fb = guard (cast n <= fb.size) $> (toList $ take n $ drop (cast (fb.size - cast n)) fb.buffer)

export infixr 5 +<
export infixr 5 +<><

export
(+<) : FiniteBuffer a -> a -> FiniteBuffer a
(FB max_size size buffer) +< x =
  let
    len = size + 1
    buf = snoc buffer x
  in
    if len > max_size then FB max_size max_size $ tail buf else FB max_size len buf

export
(+<><) : Foldable f => FiniteBuffer a -> f a -> FiniteBuffer a
buf +<>< elems = foldl (+<) buf elems

export
length : FiniteBuffer a -> Nat
length = cast . size
