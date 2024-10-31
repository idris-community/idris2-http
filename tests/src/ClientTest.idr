module ClientTest

import Network.HTTP
import Network.HTTP.URL
import Utils.String
import System.File
import System.File.Mode
import Data.Nat
import Control.Monad.Error.Either
import Control.Monad.Error.Interface

%hide Data.String.Nil

||| `fputc` in C
%foreign "C:fputc,libc"
export
prim__fputc : Int -> FilePtr -> PrimIO Int

||| `fputc` with higher level primitives in idris2
export
fputc : HasIO io => Bits8 -> File -> io (Either FileError ())
fputc b (FHandle ptr) = do
  let c = cast b
  c' <- primIO $ prim__fputc c ptr
  pure $ if c' == c then Right () else Left FileWriteError

||| Write to a `File` from a `Stream`
export
toFile : HasIO m => File -> Stream (Of Bits8) m r -> m (Either FileError r)
toFile file = fold (pure . Right) join $ \(a :> b) => do
  Right () <- fputc a file
    | Left err => pure (Left err)
  b

with_client : {e : _} -> IO (HttpClient e) -> (HttpClient e -> EitherT (HttpError e) IO a) -> EitherT (HttpError e) IO a
with_client client f = MkEitherT $ do
  c <- client
  Right ok <- runEitherT (f c)
  | Left err => close c *> pure (Left err)
  close c
  pure (Right ok)

map_error : Functor m => (e -> e') -> EitherT e m a -> EitherT e' m a
map_error f = bimapEitherT f id

export
test_redirect : EitherT String IO ()
test_redirect = map_error show $ with_client {e=()} new_client_default $ \client => do
  putStrLn "sending request"
  (response, content) <- request client GET (url' "http://openbsd.org/70.html") [] ()
  putStrLn "response header received"
  printLn response
  putStrLn "downloading response"
  content <- toList_ content
  printLn $ "\{show $ length content} bytes read"

export
test_cookie : EitherT String IO ()
test_cookie = map_error show $ with_client {e=()} (new_client certificate_ignore_check 25 5 True False) $ \client => do
  putStrLn "sending cookie set request"
  (response, content) <- request client GET (url' "https://httpbin.org/cookies/set/sessioncookie/123456789") [] ()
  putStrLn "response header received"
  printLn response
  content <- toList_ content
  printLn $ utf8_pack $ content

  putStrLn "sending cookie get request"
  (response, content) <- request client GET (url' "https://httpbin.org/cookies") [] ()
  putStrLn "response header received"
  printLn response
  content <- toList_ content
  printLn $ utf8_pack $ content

export
test_post : EitherT String IO ()
test_post = map_error show $ with_client {e=()} new_client_default $ \client => do
  putStrLn "sending cookie set request"
  let body = "this is the body"
  (response, content) <- request client POST (url' "https://httpbin.org/post") [] body
  putStrLn "response header received"
  printLn response
  content <- toList_ content
  printLn $ utf8_pack $ content

export
test_close_without_read : EitherT String IO ()
test_close_without_read = map_error show $ with_client {e=()} new_client_default $ \client => do
  putStrLn "sending request"
  (response, content) <- request client GET (url' "http://openbsd.org/70.html") [] ()
  --_ <- toList_ content -- required to pass tests
  putStrLn "response header received"
  printLn response

export
test_json_gzip : EitherT String IO ()
test_json_gzip = map_error show $ with_client {e=()} new_client_default $ \client => do
  putStrLn "sending request"
  (response, content) <- request client GET (url' "https://httpbin.org/gzip") [] ()
  putStrLn "response header received"
  printLn response
  content <- toList_ content
  putStrLn $ maybe "Nothing" id $ utf8_pack $ content
  close client

export
test_json_deflate : EitherT String IO ()
test_json_deflate = map_error show $ with_client {e=()} new_client_default $ \client => do
  putStrLn "sending request"
  (response, content) <- request client GET (url' "https://httpbin.org/deflate") [] ()
  putStrLn "response header received"
  printLn response
  content <- toList_ content
  putStrLn $ maybe "Nothing" id $ utf8_pack $ content
  close client

export
test_chunked_transfer_encoding : EitherT String IO () 
test_chunked_transfer_encoding = map_error show $ with_client {e=()} new_client_default $ \client => do
  putStrLn "sending request stream"
  (response, content) <- request client GET (url' "https://httpbin.org/stream/2") [] ()
  putStrLn "response header received"
  printLn response
  content <- toList_ content
  putStrLn $ maybe "Nothing" id $ utf8_pack $ content
  close client