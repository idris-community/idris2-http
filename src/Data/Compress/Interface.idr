module Data.Compress.Interface

public export
interface Decompressor a where
  ||| decompress a part of the compressed stream
  feed : a -> List Bits8 -> Either String (List Bits8, a)
  ||| signify the compressed stream has been completely consumed
  ||| return the leftover data
  done : a -> Either String (List Bits8)
  ||| init state
  init : a

export
decompress : a -> Decompressor a => List Bits8 -> Either String (List Bits8)
decompress _ compressed = do
  (uncompressed, state) <- feed {a} init compressed
  [] <- done state
  | x => Left "{\show (length x)} leftover bytes"
  pure uncompressed
