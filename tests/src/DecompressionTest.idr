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

%default partial

maybe_to_either : Lazy b -> Maybe a -> Either b a
maybe_to_either b Nothing = Left $ Force b
maybe_to_either _ (Just a) = Right a

new_buffer : Nat -> IO Buffer
new_buffer n = case !(newBuffer (cast n)) of Just buf => pure buf; Nothing => idris_crash "cannot make buffer"

gzcat : String -> IO (Either String ())
gzcat path = withFile path Read (pure . show) $ \file => runEitherT $ (lift $ new_buffer 128) >>= loop file init where
  loop : File -> GZipState -> Buffer -> EitherT String IO ()
  loop file state buffer = do
    False <- lift $ fEOF file
    | True => case state of MkState [] (_ ** AtGHeader) => putStrLn "\nok"; _ => idris_crash "gzip stream not complete"
    buffer_size <- lift (rawSize buffer)
    len <- bimapEitherT show id $ MkEitherT $ readBufferData file buffer 0 buffer_size
    data' <- traverse (getBits8 buffer) [0..(len-1)]
    let Right (uncompressed, state) = feed state data'
    | Left err => printLn "err: \{err}"
    lift $ putStr $ ascii_to_string uncompressed
    loop file state buffer

test_inflate_uncompressed : IO (Either String ())
test_inflate_uncompressed = gzcat "test/random.bin.gz"

test_inflate_fixed : IO (Either String ())
test_inflate_fixed = gzcat "tests/files/hello.gz"

test_inflate_text : IO (Either String ())
test_inflate_text = gzcat "tests/files/jabberwock.txt.gz"

test_inflate_jpg : IO (Either String ())
test_inflate_jpg = gzcat "tests/files/jabberwocky.jpg.gz"

test_gzip_concated : IO (Either String ())
test_gzip_concated = gzcat "tests/files/concatenated.gz"

export
test : IO ()
test = do
  Right () <- test_inflate_text
  | Left err => printLn err

  putStrLn "ok"
