module Data.Compress.Deflate

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
data DeflateParserState'
  = DeflateInit
  | DeflateHuffman
  | DeflateUncompressed
  | DeflateEnd

public export
data DeflateParserState : DeflateParserState' -> Type where
  AtHeader : DeflateParserState DeflateInit
  AtHuffman : Bool -> HuffmanTree -> DeflateParserState DeflateHuffman
  AtUncompressed : Bool -> Nat -> DeflateParserState DeflateUncompressed
  AtEnd : DeflateParserState DeflateEnd

public export
data DeflateState
  = MkState (Bitstream) (SnocList Bits8) (DPair DeflateParserState' DeflateParserState)

export
deflate_state_init : DeflateState
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

parse_deflate_uncompressed : SnocList Bits8 -> Nat -> Parser Bitstream (SimpleError String) (SnocList Bits8)
parse_deflate_uncompressed buffer len = (buffer <><) . toList <$> count len bit_getbyte

parse_deflate_huffman : SnocList Bits8 -> HuffmanTree -> Parser Bitstream (SimpleError String) (SnocList Bits8)
parse_deflate_huffman buffer tree = do
  x <- tree.parse_literals
  if x < 256 then parse_deflate_huffman (buffer :< cast x) tree
    else if x == 256 then pure buffer
    else if x < 286 then do
      let Just (off, extra) = length_lookup (cast (x - 257))
      | Nothing => fail $ msg "length symbol out of bound"
      length <- map (\b => off + cast b) (get_bits extra)
      dcode <- tree.parse_distance

      let Just (off, extra) = distance_lookup (cast dcode)
      | Nothing => fail $ msg "distance symbol out of bound"
      distance <- map (\b => off + cast b) (get_bits extra)

      let Just copied_chunk = take_last distance buffer
      | Nothing => fail $ msg "asked for distance \{show distance} but only \{show (SnocList.length buffer)} in buffer"
      let appended = take length $ stream_concat $ repeat copied_chunk
      parse_deflate_huffman (buffer <>< appended) tree
    else fail $ msg "invalid code \{show x} encountered"

parse_deflate_block : SnocList Bits8 -> Parser Bitstream (SimpleError String) (Bool, SnocList Bits8)
parse_deflate_block acc = do
  final <- token
  (final,) <$> case !(count 2 token) of
    -- Uncompressed
    [False, False] => parse_deflate_uncompressed_len >>= parse_deflate_uncompressed acc
    -- Fixed Huffman
    [True , False] => parse_deflate_huffman acc default_tree
    -- Dynamic Huffman
    [False, True ] => parse_deflate_dynamic >>= parse_deflate_huffman acc
    -- Invalid
    [True , True ] => fail $ msg "invalid compression method"

parse_deflate_header : Parser Bitstream (SimpleError String) (DPair DeflateParserState' DeflateParserState)
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

feed_deflate' : SnocList Bits8 -> DPair DeflateParserState' DeflateParserState -> Bitstream ->
                Either String (List Bits8, Maybe DeflateState)

feed_deflate' ob (DeflateEnd ** _) _ = Right ([], Nothing)

feed_deflate' ob (DeflateInit ** state) content =
  case feed content parse_deflate_header of
    Pure leftover state =>
      Right ([], Just (MkState leftover ob state))
    Fail err =>
      Left (show err)
    _ => -- underfed, need more input
      Right ([], Just (MkState content ob (_ ** AtHeader)))

feed_deflate' ob (_ ** (AtUncompressed final remaining)) content =
  let (output, leftover) = fromBits8 <$> splitAt remaining (toBits8 content)
      ob = ob <>< output
      remaining = minus remaining (length output)
      new_state =
        if remaining > 0
           then (_ ** AtUncompressed final remaining)
           else (if final then (_ ** AtEnd) else (_ ** AtHeader))
  in Right (output, Just (MkState leftover ob new_state))

feed_deflate' ob (_ ** (AtHuffman final tree)) content = ?sus

export
feed_deflate : DeflateState -> List Bits8 -> Either String (List Bits8, Maybe DeflateState)
feed_deflate (MkState ib ob state) content = feed_deflate' ob state (ib <+> fromBits8 content)

parse_deflate' : SnocList Bits8 -> Parser Bitstream (SimpleError String) (SnocList Bits8)
parse_deflate' acc = parse_deflate_block acc >>= (\(f,new) => if f then pure new else parse_deflate' new)

export
parse_deflate : Parser (List Bits8) (SimpleError String) (List Bits8)
parse_deflate = transform (Right . toBits8) (Right . fromBits8) (toList <$> parse_deflate' Lin) where
