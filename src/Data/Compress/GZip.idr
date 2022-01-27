module Data.Compress.GZip

import Data.Compress.Utils.Parser
import Data.Compress.Utils.Bytes
import Data.Compress.Utils.Misc
import Data.Vect
import Data.Bits
import Data.List
import Data.SnocList
import Data.Stream
import Control.Monad.Error.Either

import public Data.Compress.Inflate

public export
data CompressionFactor : Type where
  Poorest : CompressionFactor
  Strongest : CompressionFactor

||| It should be called file system but the RFC says it is operating system
public export
data OperatingSystem : Type where
  FAT : OperatingSystem
  Amiga : OperatingSystem
  VMS : OperatingSystem
  Unix : OperatingSystem
  VMCMS : OperatingSystem
  Atari : OperatingSystem
  HPFS : OperatingSystem
  Macintosh : OperatingSystem
  ZSystem : OperatingSystem
  CPM : OperatingSystem
  TOPS20 : OperatingSystem
  NTFS : OperatingSystem
  QDOS : OperatingSystem
  Acorn : OperatingSystem
  UnknownOS : Bits8 -> OperatingSystem

public export
record GZipHeader where
  constructor MkGZipHeader
  mtime : Bits64
  compression_factor : Maybe CompressionFactor
  os : OperatingSystem
  extra_fields : Maybe (List Bits8)
  file_name : Maybe String
  comment : Maybe String
  header_crc : Maybe (Vect 2 Bits8)

public export
data GZipParserState'
  = GZipHead
  | GZipInflate
  | GZipOrigin

public export
data GZipParserState : GZipParserState' -> Type where
  AtGHeader : GZipHeader -> GZipParserState GZipHead
  AtInflate : (isize : Bits32) -> (crc32 : Bits32) -> GZipParserState GZipInflate
  AtOrigin : GZipParserState GZipOrigin

public export
data GZipState
  = MkState (List Bits8) (DPair GZipParserState' GZipParserState)

export
gzip_state_init : GZipState
gzip_state_init = MkState [] (_ ** AtOrigin)

nul_term_string : Semigroup e => Parser (List Bits8) e String
nul_term_string = map ascii_to_string $ take_until (0 ==) token

from_id : Bits8 -> OperatingSystem
from_id  0 = FAT
from_id  1 = Amiga
from_id  2 = VMS
from_id  3 = Unix
from_id  4 = VMCMS
from_id  5 = Atari
from_id  6 = HPFS
from_id  7 = Macintosh
from_id  8 = ZSystem
from_id  9 = CPM
from_id 10 = TOPS20
from_id 11 = NTFS
from_id 12 = QDOS
from_id 13 = Acorn
from_id x  = UnknownOS x

public export
record GZipFooter where
  constructor MkGZipFooter
  crc32 : Bits32
  isize : Bits32

||| Nothing if eof
export
parse_gzip_header : Parser (List Bits8) (SimpleError String) GZipHeader
parse_gzip_header = do
  [0x1f, 0x8b] <- count 2 token
  | x => fail $ msg "gzip magic number expected, got \{show x}"
  0x08 <- token
  | x => fail $ msg "deflate method magic number expected, got \{show x}"
  flag <- token
  mtime <- count 4 token
  let mtime = le_to_integer mtime
  xfl <- token

  let compression_factor =
      case xfl of
        2 => Just Strongest
        4 => Just Poorest
        _ => Nothing

  os <- from_id <$> token

  fextra <- ifA (testBit flag 2) $ do
    xlen <- p_nat 2
    toList <$> count xlen token

  fname <- ifA (testBit flag 3) nul_term_string

  fcomment <- ifA (testBit flag 4) nul_term_string

  fhcrc <- ifA (testBit flag 1) (count 2 token)

  pure (MkGZipHeader (cast mtime) compression_factor os fextra fname fcomment fhcrc)

export
parse_gzip_footer : Parser (List Bits8) (SimpleError String) GZipFooter
parse_gzip_footer = do
  crc32 <- cast <$> p_nat 4
  isize <- cast <$> p_nat 4
  pure (MkGZipFooter crc32 isize)

export
feed_gzip : GZipState -> List Bits8 -> Either String (List Bits8, Maybe GZipState)
