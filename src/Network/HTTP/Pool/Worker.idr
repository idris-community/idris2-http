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

LogicOkOrError : Type -> Type -> Type
LogicOkOrError t_ok t_closed = Res Bool $ \ok => if ok then Handle' t_ok t_closed else t_closed

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

worker_finish : Maybe ConnectionAction -> (1 _ : Handle' t_ok t_closed) -> L1 IO (LogicOkOrError t_ok t_closed)
worker_finish (Just KeepAlive) handle =
  pure1 (True # handle)
worker_finish _ handle = do
  handle <- close handle
  pure1 (False # handle)

worker_read_fixed_length : (1 _ : Handle' t_ok t_closed) -> Integer -> Integer -> (Channel (Either (Either HttpError e) (List Bits8))) ->
                           L IO (Res Bool $ \ok => if ok then Handle' t_ok t_closed else t_closed)

worker_read_chunked : (1 _ : Handle' t_ok t_closed) -> (Channel (Either (Either HttpError e) (List Bits8))) ->
                      L IO (Res Bool $ \ok => if ok then Handle' t_ok t_closed else t_closed)

worker_logic : {e : _} -> ScheduleRequest e IO -> (1 _ : Handle' t_ok t_closed) -> L1 IO (LogicOkOrError t_ok t_closed)
worker_logic request handle = do
  let throw = \err => channelPut request.response (Left $ Left err)

  let http_message = request.raw_http_message
  let Just content_length = lookup_header http_message.headers ContentLength
  | Nothing => do
    liftIO1 $ throw (MissingHeader "Content-Length")
    pure1 (True # handle)
  (True # handle) <- write handle $ string_to_ascii $ serialize_http_message http_message
  | (False # (error # handle)) => do
    liftIO1 $ throw (SocketError error)
    pure1 (False # handle)
  (True # handle) <- worker_write handle content_length $ chunksOf 4096 request.content
  | (False # (error # handle)) => do
    liftIO1 $ throw error
    pure1 (False # handle)
  (True # (line # handle)) <- read_until_empty_line handle
  | (False # (error # handle)) => do
    liftIO1 $ throw (SocketError "error while reading response header: \{error}")
    pure1 (False # handle)
  let Right response = deserialize_http_response $ ltrim line
  | Left err => do
    handle <- close handle
    liftIO1 $ throw (SocketError "error parsing http response headers: \{err}")
    pure1 (False # handle)
  let connection_action = lookup_header response.headers Connection

  channel <- liftIO1 (makeChannel {a=(Either (Either HttpError e) (List Bits8))})
  let schedule_response = MkScheduleResponse response channel
  liftIO1 $ channelPut request.response (Right schedule_response)

  case lookup_header response.headers TransferEncoding of
    Just Chunked => do
      (True # handle) <- worker_read_chunked handle channel
      | (False # handle) => pure1 (False # handle)
      worker_finish connection_action handle
    Just (UnknownScheme x) => do
      handle <- close handle
      liftIO1 $ throw (UnknownTransferEncoding x)
      pure1 (False # handle)
    Nothing => do
      let Just content_length = lookup_header http_message.headers ContentLength
      | Nothing => do
        handle <- close handle
        liftIO1 $ throw (MissingHeader "Content-Length")
        pure1 (False # handle)
      (True # handle) <- worker_read_fixed_length handle content_length 1024 channel
      | (False # handle) => pure1 (False # handle)
      worker_finish connection_action handle

worker_loop : {e : _} -> IO () -> Fetcher e IO -> (1 _ : Handle' t_ok ()) -> L IO ()
worker_loop closer (channel ** receiver) handle = do
  request <- liftIO1 $ receiver channel
  (True # handle) <- worker_logic request handle
  | (False # ()) => liftIO1 closer
  worker_loop closer (channel ** receiver) handle

export
worker_handle : {e : _} -> Socket -> IO () -> Fetcher e IO -> (HttpError -> IO ()) -> CertificateCheck IO -> Protocol -> String -> L IO ()
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
