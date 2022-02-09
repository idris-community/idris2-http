module Test.ClientTest

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

test_redirect : IO ()
test_redirect = do
  client <- new_client_default
  putStrLn "sending request"
  Right (response, content) <- runEitherT {e=HttpError (),m=IO} $ request client GET (url' "http://openbsd.org/70.html") [] ()
  | Left err => close client *> printLn err
  putStrLn "response header received"
  printLn response
  putStrLn "downloading response"
  Right content <- runEitherT $ toList_ content
  | Left err => close client *> printLn err
  printLn $ utf8_pack $ content
  close client

test_cookie : IO ()
test_cookie = do
  client <- new_client certificate_ignore_check 25 5 True False
  putStrLn "sending cookie set request"
  Right (response, content) <- runEitherT {e=HttpError (),m=IO} $ request client GET (url' "https://httpbin.org/cookies/set/sessioncookie/123456789") [] ()
  | Left err => close client *> printLn err
  putStrLn "response header received"
  printLn response
  Right content <- runEitherT $ toList_ content
  | Left err => close client *> printLn err
  printLn $ utf8_pack $ content

  putStrLn "sending cookie get request"
  Right (response, content) <- runEitherT {e=HttpError (),m=IO} $ request client GET (url' "https://httpbin.org/cookies") [] ()
  | Left err => close client *> printLn err
  putStrLn "response header received"
  printLn response
  Right content <- runEitherT $ toList_ content
  | Left err => close client *> printLn err
  printLn $ utf8_pack $ content
  close client

test_post : IO ()
test_post = do
  client <- new_client_default
  putStrLn "sending cookie set request"
  let body = "this is the body"
  Right (response, content) <- runEitherT {e=HttpError (),m=IO} $ request client POST (url' "https://httpbin.org/post") [] body
  | Left err => close client *> printLn err
  putStrLn "response header received"
  printLn response
  Right content <- runEitherT $ toList_ content
  | Left err => close client *> printLn err
  printLn $ utf8_pack $ content
  close client

test_close_without_read : IO ()
test_close_without_read = do
  client <- new_client_default
  putStrLn "sending request"
  Right (response, content) <- runEitherT {e=HttpError (),m=IO} $ request client GET (url' "http://openbsd.org/70.html") [] ()
  | Left err => close client *> printLn err
  putStrLn "response header received"
  printLn response
  close client

test_json_gzip : IO ()
test_json_gzip = do
  client <- new_client_default
  putStrLn "sending request"
  Right (response, content) <- runEitherT {e=HttpError (),m=IO} $ request client GET (url' "https://httpbin.org/gzip") [] ()
  | Left err => close client *> printLn err
  putStrLn "response header received"
  printLn response
  Right content <- runEitherT $ toList_ content
  | Left err => close client *> printLn err
  putStrLn $ maybe "Nothing" id $ utf8_pack $ content
  close client

test_json_deflate : IO ()
test_json_deflate = do
  client <- new_client_default
  putStrLn "sending request"
  Right (response, content) <- runEitherT {e=HttpError (),m=IO} $ request client GET (url' "https://httpbin.org/deflate") [] ()
  | Left err => close client *> printLn err
  putStrLn "response header received"
  printLn response
  Right content <- runEitherT $ toList_ content
  | Left err => close client *> printLn err
  putStrLn $ maybe "Nothing" id $ utf8_pack $ content
  close client
