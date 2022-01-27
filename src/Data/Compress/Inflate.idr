module Data.Compress.Inflate

import Data.Compress.Utils.Parser
import Data.Compress.Utils.Bytes
import Data.Compress.Utils.Misc
import Data.Compress.Huffman
import Data.Vect
import Data.Bits
import Data.List
import Data.SnocList
import Data.Stream
import Control.Monad.Error.Either

public export
data InflateParserState'
  = InflateInit
  | InflateHuffman
  | InflateUncompressed
  | InflateEnd

public export
data InflateParserState : InflateParserState' -> Type where
  AtHeader : InflateParserState InflateInit
  AtHuffman : Bool -> HuffmanTree -> InflateParserState InflateHuffman
  AtUncompressed : Bool -> Nat -> InflateParserState InflateUncompressed
  AtEnd : SnocList Bits8 -> InflateParserState InflateEnd

public export
data InflateState
  = MkState Bitstream (SnocList Bits8) (DPair InflateParserState' InflateParserState)

export
deflate_state_init : InflateState
deflate_state_init = MkState neutral neutral (_ ** AtHeader)

match_off : List Nat
match_off = [ 3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258 ]

match_extra : List (Fin 32)
match_extra = [ 0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0 ]

dist_off : List Nat
dist_off = [ 1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577 ]

dist_extra : List (Fin 32)
dist_extra = [ 0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13 ]

clen_alphabets : List (Fin 19)
clen_alphabets = [ 16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15 ]

length_lookup : Nat -> Maybe (Nat, Fin 32)
length_lookup n = Just (!(index_may n match_off), !(index_may n match_extra))

distance_lookup : Nat -> Maybe (Nat, Fin 32)
distance_lookup n = Just (!(index_may n dist_off), !(index_may n dist_extra))

parse_deflate_uncompressed_len : Parser Bitstream (SimpleError String) Nat
parse_deflate_uncompressed_len = do
  len <- le_nat 2
  nlen <- cast <$> le_nat 2
  let True = (cast {to=Bits16} len) == (complement nlen)
  | False => fail $ msg "invalid length header"
  pure len

-- List (literal, code length)
parse_deflate_code_lengths : Bits32 -> Maybe Bits32 -> Parser Bitstream (SimpleError String) (Fin 19) -> ?
parse_deflate_code_lengths n_lit_code supplied_prev_length parser = loop [] 0 where
  loop : List (Bits32, Bits32) -> Bits32 -> Parser Bitstream (SimpleError String) (List (Bits32, Bits32))
  loop acc current = if current >= n_lit_code then pure acc else parser >>= \case
    16 => do
      let Just prev_length = map snd (head' acc) <|> supplied_prev_length
      | Nothing => fail $ msg "asked for previous code length, but buffer is empty"
      n <- (3 +) <$> get_bits 2
      let literals = zip [current..(current + n - 1)] (take (cast n) $ repeat prev_length)
      loop (literals <+> acc) (current + n)
    17 => do
      n <- (3 +) <$> get_bits 3
      loop acc (current + n)
    18 => do
      n <- (11 +) <$> get_bits 7
      loop acc (current + n)
    n => do
      let len = cast $ finToNat n
      loop ((current, len) :: acc) (current + 1)

parse_deflate_dynamic : Parser Bitstream (SimpleError String) HuffmanTree
parse_deflate_dynamic = do
  n_lit_code <- (257 +) <$> get_bits 5
  n_dist_code <- (1 +) <$> get_bits 5
  n_len_code <- (cast . (4 +)) <$> get_bits 4
  let True = n_len_code <= 19
  | False => fail $ msg "n_len_code exceeds 19"
  alphabets <- for (take (cast n_len_code) clen_alphabets) (\k => (k,) <$> get_bits 3)
  let Just code_length_parser = make_tree alphabets 19
  | Nothing => fail $ msg "failed to generate code length tree"

  literals <- parse_deflate_code_lengths n_lit_code Nothing code_length_parser
  let Just prev_length = map snd $ head' literals
  | Nothing => fail $ msg "literals tree parser did nothing"
  let Just literals_parser = make_tree literals (cast n_lit_code)
  | Nothing => fail $ msg "failed to generate code literals tree"

  distances <- parse_deflate_code_lengths n_dist_code (Just prev_length) code_length_parser
  let Just distances_parser = make_tree distances (cast n_dist_code)
  | Nothing => fail $ msg "failed to generate code distances tree"

  pure (MkTree literals_parser distances_parser)

data HuffmanOutput = End | Literal Bits8 | Copy Nat Nat

parse_deflate_huffman : HuffmanTree -> Parser Bitstream (SimpleError String) HuffmanOutput
parse_deflate_huffman tree = do
  x <- tree.parse_literals
  if x < 256 then pure $ Literal (cast x)
    else if x == 256 then pure End
    else if x < 286 then do
      let Just (off, extra) = length_lookup (cast (x - 257))
      | Nothing => fail $ msg "length symbol out of bound"
      length <- map (\b => off + cast b) (get_bits extra)
      dcode <- tree.parse_distance

      let Just (off, extra) = distance_lookup (cast dcode)
      | Nothing => fail $ msg "distance symbol out of bound"
      distance <- map (\b => off + cast b) (get_bits extra)
      pure $ Copy (cast length) (cast distance)
    else fail $ msg "invalid code \{show x} encountered"

parse_deflate_header : Parser Bitstream (SimpleError String) (DPair InflateParserState' InflateParserState)
parse_deflate_header = do
  final <- token
  case !(count 2 token) of
    -- Uncompressed
    [False, False] => do
      len <- parse_deflate_uncompressed_len
      pure (_ ** AtUncompressed final len)
    -- Fixed Huffman
    [True , False] => do
      pure (_ ** AtHuffman final default_tree)
    -- Dynamic Huffman
    [False, True ] => do
      tree <- parse_deflate_dynamic
      pure (_ ** AtHuffman final tree)
    -- Invalid
    [True , True ] => do
      fail $ msg "invalid compression method"

next_state : (is_final : Bool) -> DPair InflateParserState' InflateParserState
next_state True  = (_ ** AtEnd [<])
next_state False = (_ ** AtHeader)

feed_deflate' : SnocList Bits8 -> SnocList Bits8 -> DPair InflateParserState' InflateParserState ->
                Bitstream -> Either String (SnocList Bits8, InflateState)

feed_deflate' acc ob (_ ** AtEnd leftover) content =
  Right (acc, MkState neutral neutral (_ ** AtEnd (leftover <>< toBits8 content))) -- terminates

feed_deflate' acc ob (InflateInit ** state) content =
  case feed content parse_deflate_header of
    Pure leftover state =>
      feed_deflate' acc ob state leftover
    Fail err =>
      Left (show err)
    _ => -- underfed, need more input
      Right (acc, MkState content ob (_ ** AtHeader))

feed_deflate' acc ob (_ ** (AtUncompressed final remaining)) content =
  let (output, leftover) = fromBits8 <$> splitAt remaining (toBits8 content)
      ob = ob <>< output
      acc = acc <>< output
  in case minus remaining (length output) of
       S n => Right (acc, MkState leftover ob (_ ** AtUncompressed final (S n))) -- underfed
       Z   => feed_deflate' acc ob (next_state final) leftover

feed_deflate' acc' ob (_ ** (AtHuffman final tree)) content = go acc' ob tree content where
  go : SnocList Bits8 -> SnocList Bits8 -> HuffmanTree -> Bitstream -> Either String (SnocList Bits8, InflateState)
  go acc ob tree input =
    case feed input (parse_deflate_huffman tree) of
      Fail err =>
        Left (show err)
      Pure leftover End =>
        feed_deflate' acc ob (next_state final) leftover
      Pure leftover (Literal literal) =>
        go (acc :< literal) (ob :< literal) tree leftover
      Pure leftover (Copy len distance) =>
        case take_last distance ob of
          Just copied_chunk =>
            let appended = take len $ stream_concat $ repeat copied_chunk
            in go (acc <>< appended) (ob <>< appended) tree leftover
          Nothing => Left "asked for distance \{show distance} but only \{show (length ob)} in buffer"
      _ => -- underfed, need more input
        Right (acc, MkState input ob (_ ** (AtHuffman final tree)))

export
feed_deflate : InflateState -> List Bits8 -> Either String (List Bits8, InflateState)
feed_deflate (MkState ib ob state) content = mapFst toList <$> feed_deflate' Lin ob state (ib <+> fromBits8 content)

export
deflate_decompress : List Bits8 -> Either String (List Bits8)
deflate_decompress compressed =
  case feed_deflate deflate_state_init compressed of
    Left err => Left err
    Right (uncompressed, (MkState _ _ (InflateEnd ** _))) => Right uncompressed
    Right _ => Left "underfed"
