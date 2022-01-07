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
import Data.List1
import System.Concurrency
import System.Concurrency.BufferedChannel
import Control.Linear.LIO

worker_logic : ScheduleRequest e IO -> (1 _ : Handle' t_ok t_closed) ->
               L1 IO (Res Bool $ \ok => if ok then Handle' t_ok t_closed else Res String (const t_closed))

worker_loop : IO () -> Fetcher e IO -> (1 _ : Handle' t_ok ()) -> L IO ()
worker_loop closer (channel ** receiver) handle = do
  request <- liftIO1 $ receiver channel
  (True # handle) <- worker_logic request handle
  | (False # (err # ())) => liftIO1 $ closer *> channelPut request.response (Left $ Left $ SocketError err)
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
