module Data.Compress.Utils.FiniteBuffer

import Data.Linear.Array
import Data.Nat

%default total

||| An efficent buffer that stores at most n elements, 
||| will forget the oldest element once capacity has been reached
export
data FiniteBuffer : Nat -> Type -> Type where
  MkFiniteBuffer : Int -> (1 _ : LinArray a) -> FiniteBuffer (S n) a

export
new_finite_buffer : (n : Nat) -> {auto 0 ok : NonZero n} -> FiniteBuffer n a
new_finite_buffer n = let S m = n in newArray (cast n) (MkFiniteBuffer 0)

||| O(1), append an element at the end of the buffer
export
snoc : a -> (1 _ : FiniteBuffer n a) -> FiniteBuffer n a
snoc elem (MkFiniteBuffer index array) =
  let
    (size # array) = msize array
    (True # array) = write array index elem
    | (False # array) => assert_total $ assert_linear (idris_crash "index logic is wrong") array
  in
    MkFiniteBuffer ((index + 1) `mod` size) array

foldl1 : ((1 _ : acc) -> elem -> acc) -> (1 _ : acc) -> List elem -> acc
foldl1 f q [] = q
foldl1 f q (x::xs) = foldl1 f (f q x) xs

||| Append a list of elements to the end of the buffer
export
append : List a -> (1 _ : FiniteBuffer n a) -> FiniteBuffer n a
append elems buffer = foldl1 (\acc,elem => snoc elem acc) buffer elems

||| Length returns the number of elements stored in the buffer
export
length : {n : Nat} -> (1 _ : FiniteBuffer n a) -> Res Nat (const (FiniteBuffer n a))
length (MkFiniteBuffer index array) = 
  case mread array (index + 1) of
    (Just x  # arr) => (n # MkFiniteBuffer index arr)
    (Nothing # arr) => (cast index # MkFiniteBuffer index arr)

||| Take the last n elements and put them into a list
export
take : {m : Nat} -> (n : Nat) -> (1 _ : FiniteBuffer (S m) a) -> {auto 0 ok : LTE n (S m)} -> Res (List a) (const (FiniteBuffer (S m) a))
take n (MkFiniteBuffer index array) = loop [] index n (MkFiniteBuffer index array) where
  next : Int -> Int
  next x = let m' = cast (S m) in (x - 1 + m') `mod` m'

  loop : List a -> Int -> (n : Nat) -> (1 _ : FiniteBuffer (S m) a) -> Res (List a) (const (FiniteBuffer (S m) a))
  loop acc i Z buffer = (acc # buffer)
  loop acc i (S n) (MkFiniteBuffer index array) =
    let
      i = next i
      (Just elem # array) = mread array i
      | (Nothing # array) => (acc # MkFiniteBuffer index array)
    in loop (elem :: acc) i n (MkFiniteBuffer index array)

test : IO ()
test = do
  let buffer = append ["a", "b", "c", "d"] $ new_finite_buffer {a=String} 3
  let (list # buffer) = take 3 buffer
  printLn list
  putStrLn "ok"
