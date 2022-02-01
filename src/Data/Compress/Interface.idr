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

public export
data IdentityState = Id

public export
Decompressor IdentityState where
  feed Id content = Right (content, Id)
  done Id = Right []
  init = Id

infixl 10 <->

public export
(Decompressor fst, Decompressor snd) => Decompressor (snd, fst) where
  feed (f, g) content = do
    (content, g') <- feed g content
    (content, f') <- feed f content
    pure (content, (f', g'))
  done (f, g) = do
    [] <- done g
    | x => Left "{\show (length x)} leftover bytes"
    done f
  init = (init, init)

export
decompress : a -> Decompressor a => List Bits8 -> Either String (List Bits8)
decompress _ compressed = do
  (uncompressed, state) <- feed {a} init compressed
  [] <- done state
  | x => Left "{\show (length x)} leftover bytes"
  pure uncompressed
