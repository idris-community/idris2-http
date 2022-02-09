module DecompressionTest

import Data.Compress.GZip
import Data.Compress.CRC
import System.File
import System.File.Mode
import Control.Monad.Error.Either
import Control.Monad.Trans
import Data.Buffer
import Data.List
import Data.Compress.Utils.Parser
import Data.Compress.Utils.Bytes
import Data.Compress.Interface
import Data.Vect
import Data.SnocList
import Crypto.Hash
import Utils.Bytes

%default partial

maybe_to_either : Lazy b -> Maybe a -> Either b a
maybe_to_either b Nothing = Left $ Force b
maybe_to_either _ (Just a) = Right a

new_buffer : Nat -> IO Buffer
new_buffer n = case !(newBuffer (cast n)) of Just buf => pure buf; Nothing => idris_crash "cannot make buffer"

gzmd5 : String -> IO (Either String (List Bits8))
gzmd5 path = withFile path Read (pure . show) $ \file => runEitherT $ (lift $ new_buffer 128) >>= loop file init (Hash.init MD5) where
  loop : File -> GZipState -> MD5 -> Buffer -> EitherT String IO (List Bits8)
  loop file state hash_state buffer = do
    False <- lift $ fEOF file
    | True => case state of MkState [] (_ ** AtGHeader) => pure (toList $ finalize hash_state); _ => throwE "gzip stream not complete"
    buffer_size <- lift (rawSize buffer)
    len <- bimapEitherT show id $ MkEitherT $ readBufferData file buffer 0 buffer_size
    data' <- traverse (getBits8 buffer) [0..(len-1)]
    let Right (uncompressed, state) = feed state data'
    | Left err => throwE "error while decompressing: \{err}"
    loop file state (update uncompressed hash_state) buffer

gzmd5_check : String -> String -> EitherT String IO ()
gzmd5_check expected_checksum filename = do
  checksum <- MkEitherT (gzmd5 filename)
  let checksum_str = concat $ map show_hex checksum
  if checksum_str == expected_checksum
    then pure ()
    else throwE "checksum error: expected \{expected_checksum}, got \{checksum_str}"

-- Compressed data that is gzip + 1 single deflate uncompressed block
test_gzip_uncompressed : EitherT String IO ()
test_gzip_uncompressed = gzmd5_check "66bd413f7a853c4fe78bddede9e8c5d7" "files/random.bin.gz"

-- Compressed data using only fixed huffman encoding
test_gzip_fixed_huffman : EitherT String IO ()
test_gzip_fixed_huffman = gzmd5_check "0eb38a55ee9ab4f5d3283f6a33e0529e" "files/hello.gz"

-- General gzip compressed text
test_gzip_text : EitherT String IO ()
test_gzip_text = gzmd5_check "00c77291948ef959cb472e2935ca8963" "files/jabberwock.txt.gz"

-- General gzip compressed image
test_gzip_jpg : EitherT String IO ()
test_gzip_jpg = gzmd5_check "3a8f86011336fa59fe49618a05d34b83" "files/jabberwocky.jpg.gz"

-- Two gzip body concated into one file (allowed by the RFC for some reason)
test_gzip_concated : EitherT String IO ()
test_gzip_concated = gzmd5_check "ffe16dc45c35a1c3a0aa40875eca747e" "files/concatenated.gz"
