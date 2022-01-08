module Network.HTTP.Client

import Network.HTTP.Pool.ConnectionPool
import Network.HTTP.Scheduler
import Network.HTTP.Protocol
import Network.HTTP.Message
import Network.HTTP.Error
import Network.HTTP.Method
import Utils.Streaming
import Utils.Bytes
import System.File
import System.File.Mode

message : RawHttpMessage
message =
  MkRawHttpMessage GET "/"
    [ ("Host", "www.openbsd.org")
    , ("Accept", "*/*")
    , ("Content-Length", "0")
    , ("Connection", "keep-alive")
    ]

message2 : RawHttpMessage
message2 =
  MkRawHttpMessage GET "/70.html"
    [ ("Host", "www.openbsd.org")
    , ("Accept", "*/*")
    , ("Content-Length", "0")
    , ("Connection", "keep-alive")
    ]

message3 : RawHttpMessage
message3 =
  MkRawHttpMessage GET "/images/StarryPointers.png"
    [ ("Host", "www.openbsd.org")
    , ("Accept", "*/*")
    , ("Content-Length", "0")
    , ("Connection", "keep-alive")
    ]

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
  manager <- the (IO (PoolManager ())) $ new_pool_manager True
  putStrLn "sending"
  Right (response, content) <- request manager HTTP message (pure $ the (Either () ()) $ Right ())
  | Left err => printLn err
  printLn response

  putStrLn "downloading"
  -- download the thing
  content <- toList_ content
  printLn $ ascii_to_string content

  putStrLn "sending"
  Right (response, content) <- request manager HTTPS message2 (pure $ the (Either () ()) $ Right ())
  | Left err => printLn err
  printLn response

  putStrLn "downloading"
  -- download the thing
  content <- toList_ content
  printLn $ ascii_to_string content

  putStrLn "sending"
  Right (response, content) <- request manager HTTPS message3 (pure $ the (Either () ()) $ Right ())
  | Left err => printLn err
  printLn response

  putStrLn "downloading"
  x <- withFile "star.png" ReadWrite pure $ \file => do
    Right (Right x) <- toFile file content
    | Right (Left err) => printLn err $> Right ()
    | Left err => printLn err $> Left err
    pure $ Right x

  printLn x
  putStrLn "closing"
  evict_all {e=(), m=IO} manager
