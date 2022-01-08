module Network.HTTP.Pool.IOStuff

import Utils.Handle
import Utils.Bytes
import Utils.Misc
import Utils.Handle.C
import Control.Linear.LIO
import Network.Socket
import Data.String
import Data.String.Extra
import Network.HTTP.Message
import Network.HTTP.Header
import Data.Nat
import Data.Vect
import System.Future
import System
import Data.Fin

%hide Network.Socket.close

public export
OkOrError : Type -> Type -> Type
OkOrError t_ok t_closed = Res Bool $ \ok => if ok then Res String (const $ Handle' t_ok t_closed) else Res String (const t_closed)

public export
read_line' : LinearIO m =>
             List Char ->
             (1 _ : Handle' t_ok t_closed) ->
             L1 m $ OkOrError t_ok t_closed
read_line' acc handle = do
  (True # ([char] # handle)) <- read handle 1
  | (True # (char # handle)) => do
    handle' <- close handle
    pure1 (False # ("read line failed, somehow read returns \{show (length char)} bytes instead of 1" # handle'))
  | (False # (error # handle)) => pure1 (False # ("read line failed: \{error}" # handle))
  case cast {to=Char} char of
    '\n' => pure1 (True # ((pack $ reverse acc) # handle))
    '\r' => do
      (True # ([10] # handle)) <- read handle 1
      | (True # ([char] # handle)) => do
        handle' <- close handle
        pure1 (False # ("read line failed, \\n expected after \\r, got \{show char} instead" # handle'))
      | (True # (char # handle)) => do
        handle' <- close handle
        pure1 (False # ("read line failed, somehow read returns \{show (length char)} bytes instead of 1" # handle'))
      | (False # (error # handle)) => pure1 (False # ("read line failed: \{error}" # handle))
      pure1 (True # ((pack $ reverse acc) # handle))
    x => read_line' (x :: acc) handle

public export
read_line : LinearIO m => (1 _ : Handle' t_ok t_closed) -> L1 m $ OkOrError t_ok t_closed
read_line = read_line' []

public export
read_until_empty_line' : String -> LinearIO m => (1 _ : Handle' t_ok t_closed) -> L1 m $ OkOrError t_ok t_closed
read_until_empty_line' acc handle = do
  (True # (line # handle)) <- read_line handle
  | (False # (error # handle)) => pure1 (False # ("read line failed: \{error}" # handle))
  if null line then pure1 (True # (acc # handle)) else read_until_empty_line' (acc <+> "\n" <+> line) handle

public export
read_until_empty_line : LinearIO m => (1 _ : Handle' t_ok t_closed) -> L1 m $ OkOrError t_ok t_closed
read_until_empty_line = read_until_empty_line' ""

public export
send_and_recv_http_body : LinearIO m => List Bits8 -> (1 _ : Handle' t_ok t_closed) -> L1 m $ Res Bool $ \ok =>
                          if ok then Res (Nat, RawHttpResponse) (const $ Handle' t_ok t_closed) else Res String (const t_closed)
send_and_recv_http_body body handle = do
  (True # handle) <- write handle body
  | (False # error) => pure1 (False # error)

  (True # (line # handle)) <- read_until_empty_line handle
  | (False # error) => pure1 (False # error)

  let Right response = deserialize_http_response $ ltrim line
  | Left err => do
    handle' <- close handle
    pure1 (False # ("error parsing http response headers: \{err}" # handle'))

  let Just content_length = lookup_header response.headers ContentLength
  | Nothing => do
    handle' <- close handle
    pure1 (False # ("error: cannot find Content-Length" # handle'))

  pure1 (True # ((integerToNat content_length, response) # handle))

test_http_body : String -> List Bits8
test_http_body hostname =
  string_to_ascii
  $ join "\r\n"
  $ the (List String)
  [ "GET / HTTP/1.1"
  , "Host: " <+> hostname
  , "Connection: keep-alive"
  , "User-Agent: curl"
  , "Accept: */*"
  , "Content-Length: 0"
  , ""
  , ""
  ]

test_http_body2 : String -> List Bits8
test_http_body2 hostname =
  string_to_ascii
  $ join "\r\n"
  $ the (List String)
  [ "GET /70.html HTTP/1.1"
  , "Host: " <+> hostname
  , "Connection: keep-alive"
  , "User-Agent: curl"
  , "Accept: */*"
  , "Content-Length: 0"
  , ""
  , ""
  ]

export
test : String -> Int -> IO ()
test target_hostname port = do
  Right sock <- socket AF_INET Stream 0
  | Left err => putStrLn $ "unable to create socket: " <+> show err
  0 <- connect sock (Hostname target_hostname) port
  | _ => putStrLn "unable to connect"
  
  -- Here we begin TLS communication in a linear fasion
  run $ do
    let handle = socket_to_handle sock
    -- first http
    (True # ((length, response) # handle)) <- send_and_recv_http_body (test_http_body target_hostname) handle
    | (False # (error # ())) => putStrLn error

    putStrLn $ show response
    (True # (content # handle)) <- read handle length
    | (False # (error # ())) => putStrLn error

    -- putStrLn $ ascii_to_string content
  
    -- second http
    (True # ((length, response) # handle)) <- send_and_recv_http_body (test_http_body2 target_hostname) handle
    | (False # (error # ())) => putStrLn error

    putStrLn $ show response
    (True # (content # handle)) <- read handle length
    | (False # (error # ())) => putStrLn error

    -- putStrLn $ ascii_to_string content

    close handle
    putStrLn "ok"

export
test_future : IO ()
test_future = do
  let vect = the (Vect 5 Int) $ map (cast . finToInteger) Fin.range
  threads <- traverse (\x => forkIO (sleep x *> putStrLn "\{show x}" $> x)) vect
  putStrLn $ show $ map await threads
  putStrLn "done"
