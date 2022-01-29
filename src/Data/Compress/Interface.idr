module Data.Compress.Interface

public export
interface Decompressor a where
  ||| decompress a part of the compressed stream
  feed : a -> List Bits8 -> Either String (List Bits8, a)
  ||| signify the compressed stream has been completely consumed
  done : a -> Either String (List Bits8)
