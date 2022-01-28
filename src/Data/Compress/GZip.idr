module Data.Compress.GZip

import Data.Compress.Utils.Parser
import Data.Compress.Utils.Bytes
import Data.Compress.Utils.Misc
import Data.Vect
import Data.Bits
import Data.List
import Data.SnocList
import Data.Stream
import Data.Compress.CRC
import Control.Monad.Error.Either

import public Data.Compress.Inflate

public export
data GZipParserState'
  = GZipHead
  | GZipFoot
  | GZipInflate

public export
data GZipParserState : GZipParserState' -> Type where
  AtGHeader : GZipParserState GZipHead
  AtInflate : (isize : Bits32) -> (crc32 : Bits32) -> InflateState -> GZipParserState GZipInflate
  AtGFooter : (isize : Bits32) -> (crc32 : Bits32) -> GZipParserState GZipFoot

public export
data GZipState
  = MkState (List Bits8) (DPair GZipParserState' GZipParserState)

export
gzip_state_init : GZipState
gzip_state_init = MkState [] (_ ** AtGHeader)

nul_term_string : Semigroup e => Parser (List Bits8) e String
nul_term_string = map ascii_to_string $ take_until (0 ==) token

record GZipFooter where
  constructor MkGZipFooter
  crc32 : Bits32
  isize : Bits32

export
parse_gzip_header : Parser (List Bits8) (SimpleError String) ()
parse_gzip_header = do
  [0x1f, 0x8b] <- count 2 token
  | x => fail $ msg "gzip magic number expected, got \{show x}"
  0x08 <- token
  | x => fail $ msg "deflate method magic number expected, got \{show x}"
  flag <- token
  mtime <- count 4 token
  xfl <- token
  os <- token

  fextra <- ifA (testBit flag 2) $ do
    xlen <- p_nat 2
    toList <$> count xlen token

  fname <- ifA (testBit flag 3) nul_term_string

  fcomment <- ifA (testBit flag 4) nul_term_string

  fhcrc <- ifA (testBit flag 1) (count 2 token)

  pure ()

export
parse_gzip_footer : Parser (List Bits8) (SimpleError String) GZipFooter
parse_gzip_footer = do
  crc32 <- cast <$> p_nat 4
  isize <- cast <$> p_nat 4
  pure (MkGZipFooter crc32 isize)

feed_gzip' : SnocList Bits8 -> DPair GZipParserState' GZipParserState -> List Bits8 -> Either String (SnocList Bits8, GZipState)
feed_gzip' acc (_ ** AtGHeader) [] = Right (acc, MkState [] (_ ** AtGHeader))
feed_gzip' acc (_ ** AtGHeader) content =
  case feed content parse_gzip_header of
    Pure leftover header => feed_gzip' acc (_ ** AtInflate 0 0 inflate_state_init) leftover
    Fail err => Left $ show err
    _ => Right (acc, MkState content (_ ** AtGHeader))
feed_gzip' acc (_ ** AtInflate isize crc32 inflate_state) content =
  case feed_inflate inflate_state content of
    Left err => Left err
    Right (uncompressed, (MkState _ _ (_ ** AtEnd leftover))) =>
      let isize = isize + (cast $ length uncompressed)
          crc32 = update_crc32 crc32 uncompressed
      in feed_gzip' (acc <>< uncompressed) (_ ** AtGFooter isize crc32) (toList leftover)
    Right (uncompressed, inflate_state) => -- underfed
      let isize = isize + (cast $ length uncompressed)
          crc32 = update_crc32 crc32 uncompressed
      in Right (acc <>< uncompressed, MkState [] (_ ** AtInflate isize crc32 inflate_state))
feed_gzip' acc (_ ** AtGFooter isize crc32) content =
  case feed content parse_gzip_footer of
    Pure leftover footer =>
      if footer.crc32 /= crc32 then Left "crc32 mismatch"
        else if footer.isize /= isize then Left "isize mismatch"
        else feed_gzip' acc (_ ** AtGHeader) leftover
    Fail err => Left $ show err
    _ => Right (acc, MkState content (_ ** AtGFooter isize crc32))

export
feed_gzip : GZipState -> List Bits8 -> Either String (List Bits8, GZipState)
feed_gzip (MkState leftover state) input = mapFst toList <$> feed_gzip' Lin state (leftover <+> input)

export
gzip_decompress : List Bits8 -> Either String (List Bits8)
gzip_decompress compressed =
  case feed_gzip gzip_state_init compressed of
    Left err => Left err
    Right (uncompressed, (MkState [] (_ ** AtGHeader))) => Right uncompressed
    Right _ => Left "underfed"
