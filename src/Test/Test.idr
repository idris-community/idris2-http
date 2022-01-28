module Test.Test

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
import Data.Vect
import Data.SnocList

%default partial

maybe_to_either : Lazy b -> Maybe a -> Either b a
maybe_to_either b Nothing = Left $ Force b
maybe_to_either _ (Just a) = Right a

readFile : File -> EitherT String IO (List Bits8)
readFile file = (MkEitherT $ map (maybe_to_either "canont create buffer") (newBuffer 1024)) >>= loop Lin where
  loop : SnocList Bits8 -> Buffer -> EitherT String IO (List Bits8)
  loop acc buffer = do
    False <- lift $ fEOF file
    | True => pure (toList acc)
    buffer_size <- lift (rawSize buffer)
    len <- bimapEitherT show id $ MkEitherT $ readBufferData file buffer 0 buffer_size
    data' <- traverse (getBits8 buffer) [0..(len-1)]
    loop (acc <>< data') buffer

test_gzip_file : String -> IO (Either String ())
test_gzip_file path = withFile path Read (pure . show) $ \file => runEitherT $ do
  content <- readFile file
  let Right uncompressed = gzip_decompress content
  | Left err => printLn err
  putStrLn $ ascii_to_string uncompressed
  putStrLn ""

test_inflate_uncompressed : IO (Either String ())
test_inflate_uncompressed = test_gzip_file "test/random.bin.gz"

test_inflate_fixed : IO (Either String ())
test_inflate_fixed = test_gzip_file "test/hello.gz"

test_inflate_text : IO (Either String ())
test_inflate_text = test_gzip_file "test/jabberwock.txt.gz"

test_inflate_jpg : IO (Either String ())
test_inflate_jpg = test_gzip_file "test/jabberwocky.jpg.gz"

export
test : IO ()
test = do
  Right () <- test_inflate_text
  | Left err => printLn err

  putStrLn "ok"
