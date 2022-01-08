module Network.HTTP.Pool.Worker

import Network.Socket
import Network.HTTP.Error
import Network.HTTP.Scheduler
import Network.HTTP.Protocol
import Network.HTTP.Message
import Network.HTTP.Header
import Network.HTTP.URL
import Network.HTTP.Pool.Common
import Network.TLS
import Network.TLS.Signature
import Crypto.Random
import Crypto.Random.C
import Utils.Handle.C
import Utils.Handle
import Utils.Bytes
import Utils.Streaming
import Data.List1
import Data.Nat
import Data.String
import System.Concurrency
import System.Concurrency.BufferedChannel
import Control.Linear.LIO
import Network.HTTP.Pool.IOStuff

-- needed for some reason
%hide Network.Socket.close

WriteOkOrError : Type -> Type -> Type
WriteOkOrError t_ok t_closed = Res Bool $ \ok => if ok then Handle' t_ok t_closed else Res HttpError (const t_closed)

worker_write : (1 _ : Handle' t_ok t_closed) -> Integer -> Stream (Of (List Bits8)) IO (Either e ()) -> L1 IO (WriteOkOrError t_ok t_closed)
worker_write handle remaining stream = do
  Right (chunk, rest) <- liftIO1 $ next stream
  | Left (Left r) => do
    handle <- close handle
    pure1 (False # ((SocketError "error while writing to server") # handle))
  | Left (Right ()) => if remaining <= 0 then pure1 (True # handle) else do
    handle <- close handle
    pure1 (False # ((ContentLengthMismatch remaining) # handle))
  let should_take = min remaining (natToInteger $ length chunk)
  let chunk = take (integerToNat should_take) chunk
  (True # handle) <- write handle chunk
  | (False # (error # handle)) => pure1 (False # ((SocketError error) # handle))
  worker_write handle (remaining - should_take) rest

worker_logic : ScheduleRequest e IO -> (1 _ : Handle' t_ok t_closed) ->
               L1 IO (Res Bool $ \ok => if ok
                     then Res (Maybe HttpError) (const $ Handle' t_ok t_closed)
                     else Res (Maybe HttpError) (const t_closed))
worker_logic request handle = do
  let http_message = request.raw_http_message
  let Just content_length = lookup_header http_message.headers ContentLength
  | Nothing => pure1 (True # (Just MissingContentLength # handle))
  (True # handle) <- write handle $ string_to_ascii $ serialize_http_message http_message
  | (False # (error # handle)) => pure1 (False # (Just (SocketError error) # handle))
  (True # handle) <- worker_write handle content_length $ chunksOf 4096 request.content
  | (False # (error # handle)) => pure1 (False # (Just error # handle))
  (True # (line # handle)) <- read_until_empty_line handle
  | (False # (error # handle)) => pure1 (False # (Just (SocketError "error while reading response header: \{error}") # handle))
  let Right response = deserialize_http_response $ ltrim line
  | Left err => do
    handle <- close handle
    pure1 (False # (Just (SocketError "error parsing http response headers: \{err}") # handle))
  ?read_stuff
  case lookup_header response.headers Connection of
    Just KeepAlive => do
      handle <- close handle
      pure1 (False # (Nothing # handle))
    _ =>
      pure1 (True # (Nothing # handle))

worker_loop : IO () -> Fetcher e IO -> (1 _ : Handle' t_ok ()) -> L IO ()
worker_loop closer (channel ** receiver) handle = do
  request <- liftIO1 $ receiver channel
  (True # (mayerr # handle)) <- worker_logic request handle
  | (False # (Nothing # ())) => liftIO1 closer
  | (False # (Just err # ())) => liftIO1 $ closer *> channelPut request.response (Left $ Left err)
  case mayerr of
    Just err => do
      liftIO1 $ channelPut request.response (Left $ Left err)
      worker_loop closer (channel ** receiver) handle
    Nothing =>
      worker_loop closer (channel ** receiver) handle

export
worker_handle : Socket -> IO () -> Fetcher e IO -> (HttpError -> IO ()) -> CertificateCheck IO -> Protocol -> String -> L IO ()
worker_handle socket closer fetcher throw cert_checker protocol hostname = do
  let handle = socket_to_handle socket
  case protocol of
    HTTP => worker_loop closer fetcher handle
    HTTPS => do
      (True # handle) <-
        tls_handshake hostname
          (X25519 ::: [SECP256r1, SECP384r1])
          supported_signature_algorithms
          (tls13_supported_cipher_suites <+> tls12_supported_cipher_suites)
          handle
          cert_checker
      | (False # (err # ())) => liftIO1 $ throw $ SocketError "error during TLS handshake: \{err}"
      worker_loop closer fetcher handle
