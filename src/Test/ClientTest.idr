module Test.ClientTest

import Network.HTTP
import Network.HTTP.URL
import Utils.String
import System.File
import System.File.Mode
import System

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
toFile file = build (pure . Right) join $ \(a :> b) => do
  Right () <- fputc a file
    | Left err => pure (Left err)
  b

test : IO ()
test = do
  client <- new_client {e=()} certificate_ignore_check 25 5 True True
  putStrLn "sending request"
  Right (response, content) <- request client GET (url' "http://openbsd.org/70.html") [] ()
  | Left err => close client *> printLn err
  putStrLn "response header received"
  printLn response
  putStrLn "downloading response"
  content <- toList_ content
  printLn $ utf8_pack $ content
  close client
  -- give client ample time to close
  usleep 5000

test_cookie : IO ()
test_cookie = do
  client <- new_client {e=()} certificate_ignore_check 25 5 True False
  putStrLn "sending cookie set request"
  Right (response, content) <- request client GET (url' "https://httpbin.org/cookies/set/sessioncookie/123456789") [] ()
  | Left err => close client *> printLn err
  putStrLn "response header received"
  printLn response
  content <- toList_ content
  printLn $ utf8_pack $ content

  putStrLn "sending cookie get request"
  Right (response, content) <- request client GET (url' "https://httpbin.org/cookies") [] ()
  | Left err => close client *> printLn err
  putStrLn "response header received"
  printLn response
  content <- toList_ content
  printLn $ utf8_pack $ content
  close client
  -- give client ample time to close
  usleep 5000

test_post : IO ()
test_post = do
  client <- new_client {e=()} certificate_ignore_check 25 5 True True
  putStrLn "sending cookie set request"
  let body = "this is the body"
  Right (response, content) <- request client POST (url' "https://httpbin.org/post") [] body
  | Left err => close client *> printLn err
  putStrLn "response header received"
  printLn response
  content <- toList_ content
  printLn $ utf8_pack $ content

  close client
  -- give client ample time to close
  usleep 5000
