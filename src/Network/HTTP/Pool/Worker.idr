module Network.HTTP.Pool.Worker

import Network.Socket
import Network.HTTP.Error
import Network.HTTP.Scheduler
import Network.HTTP.Protocol
import Network.HTTP.Message
import Network.HTTP.Header
import Network.HTTP.URL
import Network.HTTP.Pool.Common
import Utils.String
import Network.TLS
import Network.TLS.Signature
import Crypto.Random
import Crypto.Random.C
import Utils.Handle.C
import Utils.Handle
import Utils.Bytes
import Utils.Streaming
import Utils.Num
import Data.List1
import Data.Nat
import Data.Fin
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

worker_read_fixed_length : (1 _ : Handle' t_ok t_closed) -> Integer -> Integer -> Channel (Either (Either HttpError e) (Maybe (List Bits8))) ->
                           L1 IO (LogicOkOrError t_ok t_closed)
worker_read_fixed_length handle remaining chunk_size channel =
  if remaining <= 0
  then do
    liftIO1 $ channelPut channel (Right Nothing)
    pure1 (True # handle)
  else do
    let should_read = min remaining chunk_size
    (True # (content # handle)) <- read handle (integerToNat should_read)
    | (False # (error # handle)) => do
      liftIO1 $ channelPut channel (Left $ Left $ SocketError "error while reading response: \{error}")
      pure1 (False # handle)
    liftIO1 $ channelPut channel (Right $ Just content)
    worker_read_fixed_length handle (remaining - should_read) chunk_size channel

worker_read_chunked_end : (1 _ : Handle' t_ok t_closed) -> Channel (Either (Either HttpError e) (Maybe (List Bits8))) ->
                          L1 IO (LogicOkOrError t_ok t_closed)
worker_read_chunked_end handle channel = do
  (True # ([char] # handle)) <- read handle 1
  | (True # (char # handle)) => do
    handle' <- close handle
    liftIO1 $ channelPut channel (Left $ Left $ SocketError "read chunked body failed, somehow read returns \{show (length char)} bytes instead of 1")
    pure1 (False # handle')
  | (False # (error # handle)) => do
    liftIO1 $ channelPut channel (Left $ Left $ SocketError "read chunked body failed: \{error}")
    pure1 (False # handle)
  case cast {to=Char} char of
    '\n' =>
      pure1 (True # handle)
    '\r' => do
      (True # ([10] # handle)) <- read handle 1
      | (True # ([char] # handle)) => do
        handle' <- close handle
        liftIO1 $ channelPut channel (Left $ Left $ SocketError "read chunked body failed, \\n expected after \\r, got \{show char} instead")
        pure1 (False # handle')
      | (True # (char # handle)) => do
        handle' <- close handle
        liftIO1 $ channelPut channel (Left $ Left $ SocketError "read chunked body failed, somehow read returns \{show (length char)} bytes instead of 1")
        pure1 (False # handle')
      | (False # (error # handle)) => do
        liftIO1 $ channelPut channel (Left $ Left $ SocketError "read chunked body failed: \{error}")
        pure1 (False # handle)
      pure1 (True # handle)
    chr => do
      handle <- close handle
      liftIO1 $ channelPut channel (Left $ Left $ SocketError "read chunked body tail failed: got \{show chr} instead of \\n or \\r")
      pure1 (False # handle)

worker_read_chunked : (1 _ : Handle' t_ok t_closed) -> Channel (Either (Either HttpError e) (Maybe (List Bits8))) ->
                      L1 IO (LogicOkOrError t_ok t_closed)
worker_read_chunked handle channel = do
  (True # (length_line # handle)) <- read_line handle
  | (False # (error # handle)) => do
    liftIO1 $ channelPut channel (Left $ Left $ SocketError "error while reading response: \{error}")
    pure1 (False # handle)
  let Just (S len) = stringToNat 16 $ toLower length_line
  | Just Z => do
    liftIO1 $ channelPut channel (Right Nothing)
    worker_read_chunked_end handle channel
  | Nothing => do
    handle <- close handle
    liftIO1 $ channelPut channel (Left $ Left $ SocketError "invalid chunked header: \{length_line}")
    pure1 (False # handle)
  (True # (content # handle)) <- read handle (S len)
  | (False # (error # handle)) => do
    liftIO1 $ channelPut channel (Left $ Left $ SocketError "error while reading chunked body: \{error}")
    pure1 (False # handle)

  liftIO1 $ channelPut channel (Right $ Just content)
  worker_read_chunked_end handle channel

worker_logic : {e : _} -> ScheduleRequest e IO -> (1 _ : Handle' t_ok t_closed) -> L1 IO (LogicOkOrError t_ok t_closed)
worker_logic request handle = do
  let throw = \err => channelPut request.response (Left $ Left err)

  let http_message = request.raw_http_message
  let Just content_length = lookup_header http_message.headers ContentLength
  | Nothing => do
    liftIO1 $ throw (MissingHeader "Content-Length")
    pure1 (True # handle)
  (True # handle) <- write handle $ utf8_unpack $ serialize_http_message http_message
  | (False # (error # handle)) => do
    liftIO1 $ throw (SocketError error)
    pure1 (False # handle)
  (True # handle) <- worker_write handle content_length $ chunksOf 0x200 request.content
  | (False # (error # handle)) => do
    liftIO1 $ throw error
    pure1 (False # handle)
  (True # (line # handle)) <- read_until_empty_line handle
  | (False # (error # handle)) => do
    liftIO1 $ throw (SocketError "error while reading response header: \{error}")
    pure1 (False # handle)

  let Right response = deserialize_http_response $ (ltrim line <+> "\n") -- for some reason the end line sometimes is not sent
  | Left err => do
    handle <- close handle
    liftIO1 $ throw (SocketError "error parsing http response headers: \{err}")
    pure1 (False # handle)
  let connection_action = lookup_header response.headers Connection

  channel <- liftIO1 (makeChannel {a=(Either (Either HttpError e) (Maybe (List Bits8)))})
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
      let Just content_length = lookup_header response.headers ContentLength
      | Nothing => do
        handle <- close handle
        liftIO1 $ throw (MissingHeader "Content-Length")
        pure1 (False # handle)
      (True # handle) <- worker_read_fixed_length handle content_length 0x2000 channel
      | (False # handle) => pure1 (False # handle)
      worker_finish connection_action handle

worker_loop : {e : _} -> IORef Bool -> IO () -> Fetcher e -> (1 _ : Handle' t_ok ()) -> L IO ()
worker_loop idle_ref closer (channel ** receiver) handle = do
  liftIO1 $ writeIORef idle_ref True
  Request request <- liftIO1 $ receiver channel
  | Kill condition => do
    close handle
    liftIO1 closer
    case condition of
      Just cond => liftIO1 $ conditionBroadcast cond
      Nothing => pure ()
  liftIO1 $ writeIORef idle_ref False
  (True # handle) <- worker_logic request handle
  | (False # ()) => liftIO1 closer
  worker_loop idle_ref closer (channel ** receiver) handle

export
worker_handle : {e : _} -> Socket -> IORef Bool -> IO () -> Fetcher e -> (HttpError -> IO ()) ->
                (String -> CertificateCheck IO) -> Protocol -> String -> IO ()
worker_handle socket idle_ref closer fetcher throw cert_checker protocol hostname = run $ do
  let handle = socket_to_handle socket
  case protocol of
    HTTP =>
      worker_loop idle_ref closer fetcher handle
    HTTPS => do
      (True # handle) <-
        tls_handshake hostname
          (X25519 ::: [SECP256r1, SECP384r1])
          supported_signature_algorithms
          (tls13_supported_cipher_suites <+> tls12_supported_cipher_suites)
          handle
          (cert_checker hostname)
      | (False # (err # ())) => liftIO1 $ throw $ SocketError "error during TLS handshake: \{err}"
      worker_loop idle_ref closer fetcher handle
