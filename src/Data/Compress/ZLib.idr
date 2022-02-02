module Data.Compress.ZLib

import Data.Compress.Utils.Parser
import Data.Compress.Utils.Bytes
import Data.Compress.Utils.Misc
import Data.Compress.Interface
import Data.Vect
import Data.Bits
import Data.List
import Data.SnocList
import Data.Stream
import Data.Compress.CRC
import Control.Monad.Error.Either

import public Data.Compress.Inflate

public export
data ZLibParserState'
  = ZLibHead
  | ZLibFoot
  | ZLibInflate

public export
data ZLibParserState : ZLibParserState' -> Type where
  AtZHeader : ZLibParserState ZLibHead
  AtInflate : (adler32 : Bits32) -> InflateState -> ZLibParserState ZLibInflate
  AtZFooter : (adler32 : Bits32) -> ZLibParserState ZLibFoot

public export
data ZLibState
  = MkState (List Bits8) (DPair ZLibParserState' ZLibParserState)

nul_term_string : Semigroup e => Parser (List Bits8) e String
nul_term_string = map ascii_to_string $ take_until (0 ==) token

record ZLibFooter where
  constructor MkZLibFooter
  adler32 : Bits32

export
parse_zlib_header : Parser (List Bits8) (SimpleError String) ()
parse_zlib_header = do
  cmf <- token
  flg <- token

  let True = 0 == (((shiftL {a=Bits16} (cast cmf) 8) .|. cast flg) `mod` 31)
  | False => fail $ msg "zlib: fcheck checksum failed"

  let False = testBit flg 5
  | True => fail $ msg "zlib: fdict is set"

  pure ()

update_adler32 : Bits32 -> List Bits8 -> Bits32
update_adler32 alder = go (alder .&. 0xffff) (shiftR alder 16) where
  go : Bits32 -> Bits32 -> List Bits8 -> Bits32
  go a b (x :: xs) =
    let base = 65521
        a' = (a + (cast x)) `mod` base
        b' = (a' + b) `mod` base
    in go a' b' xs
  go a b [] = (shiftL b 16) .|. a

export
parse_zlib_footer : Parser (List Bits8) (SimpleError String) ZLibFooter
parse_zlib_footer = do
  adler32 <- cast <$> p_be_nat 4
  pure (MkZLibFooter adler32)

feed_zlib' : SnocList Bits8 -> DPair ZLibParserState' ZLibParserState -> List Bits8 -> Either String (SnocList Bits8, ZLibState)
feed_zlib' acc (_ ** AtZHeader) [] = Right (acc, MkState [] (_ ** AtZHeader))
feed_zlib' acc (_ ** AtZHeader) content =
  case feed content parse_zlib_header of
    Pure leftover header => feed_zlib' acc (_ ** AtInflate 1 init) leftover
    Fail err => Left $ show err
    _ => Right (acc, MkState content (_ ** AtZHeader))
feed_zlib' acc (_ ** AtInflate adler32 inflate_state) content =
  case feed inflate_state content of
    Left err => Left err
    Right (uncompressed, (MkState _ _ (_ ** AtEnd leftover))) =>
      let adler32 = update_adler32 adler32 uncompressed
      in feed_zlib' (acc <>< uncompressed) (_ ** AtZFooter adler32) (toList leftover)
    Right (uncompressed, inflate_state) => -- underfed
      let adler32 = update_adler32 adler32 uncompressed
      in Right (acc <>< uncompressed, MkState [] (_ ** AtInflate adler32 inflate_state))
feed_zlib' acc (_ ** AtZFooter adler32) content =
  case feed content parse_zlib_footer of
    Pure leftover footer =>
      if footer.adler32 /= adler32
         then Left "adler32 mismatch \{show footer.adler32} \{show adler32}"
        else feed_zlib' acc (_ ** AtZHeader) leftover
    Fail err => Left $ show err
    _ => Right (acc, MkState content (_ ** AtZFooter adler32))

export
Decompressor ZLibState where
  feed (MkState leftover state) input = mapFst toList <$> feed_zlib' Lin state (leftover <+> input)
  done (MkState [] (_ ** AtZHeader)) = Right []
  done (MkState _ (_ ** AtZHeader)) = Left "zlib: leftover data after header"
  done _ = Left "zlib: underfed"
  init = MkState [] (_ ** AtZHeader)
