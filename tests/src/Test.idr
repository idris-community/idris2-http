module Test

import Data.List
import System

import DecompressionTest
import ClientTest
import Control.Monad.Error.Either

%default partial

run_test : String -> EitherT String IO () -> IO Bool
run_test name test = do
  Right () <- runEitherT test
  | Left err => putStrLn "\{name}: failed \{err}" $> False
  putStrLn "\{name}: success" $> True

run : List (IO Bool) -> IO ()
run tests = do
  results <- sequence tests
  let [] = filter not results
  | xs => putStrLn "\{show (length xs)} tests failed" *> exitFailure
  putStrLn "all tests passed"

export
main : IO ()
main = run 
  [ --run_test "decompress random.bin.gz" test_gzip_uncompressed
  -- , run_test "decompress hello.gz" test_gzip_fixed_huffman
  -- , run_test "decompress jabberwock.txt.gz" test_gzip_text
  -- , run_test "decompress jabberwocky.jpg.gz" test_gzip_jpg
  -- , run_test "decompress concatenated.gz" test_gzip_concated
  -- , run_test "http close w/out read" test_close_without_read
  -- , run_test "http cookie jar" test_cookie
  -- , run_test "http httpbin deflate" test_json_deflate
  -- , run_test "http httpbin gzip" test_json_gzip
  -- , run_test "http httpbin post" test_post
  -- , run_test "http openbsd redirect" test_redirect
   run_test "chunked transfer encoding" test_chunked_transfer_encoding
 ]
