module Network.HTTP.Client

import Network.HTTP.Pool.ConnectionPool
import Network.HTTP.Scheduler
import Network.HTTP.Protocol
import Network.HTTP.Message
import Network.HTTP.Error
import Network.HTTP.Header
import Network.HTTP.Method
import Network.HTTP.URL
import Network.HTTP.Path
import Network.HTTP.Status
import Network.HTTP.Cookie
import Network.TLS
import Network.TLS.Certificate.System
import Network.TLS.Signature
import Utils.Streaming
import Utils.String
import Utils.Bytes
import Control.Monad.Error.Either
import Control.Monad.Error.Interface
import Control.Monad.Trans
import Data.String
import Data.String.Extra
import Data.Nat
import Data.IORef
import Decidable.Equality

public export
record HttpClient e where
  constructor MkHttpClient
  cookie_jar : IORef CookieJar
  store_cookie : Bool
  follow_redirect : Bool
  pool_manager : PoolManager e

||| Close all existing connections in the HTTP client.
||| A closed client can be reused again.
export
close : {e : _} -> HasIO io => HttpClient e -> io ()
close client = liftIO $ evict_all {m=IO,e=e} $ client.pool_manager

||| Creates a new HTTP client.
||| Arguments:
|||
||| @ cert_checker the function used to verify the website's TLS certificate.
|||                `certificate_check` and `certificate_ignore_check` are provided in `Network.TLS`.
||| @ max_total_connection the maximum alive connections (per protocol) allowed
||| @ max_per_site_connection the maximum alive connections (per protocol) per site allowed
||| @ store_cookie whether the client should store received cookies
||| @ follow_redirect whether the client should follow redirects according to response status codes
export
new_client : HasIO io => (String -> CertificateCheck IO) ->
             (max_total_connection : Nat) -> {auto 0 n01 : NonZero max_total_connection} ->
             (max_per_site_connection: Nat) -> {auto 0 no2 : NonZero max_per_site_connection} ->
             {auto 0 lte : LTE max_per_site_connection max_total_connection} ->
             Bool -> Bool -> io (HttpClient e)
new_client cert_checker max_total_connection max_per_site_connection store_cookie follow_redirect = do
  manager <- new_pool_manager' max_per_site_connection max_total_connection cert_checker
  jar <- newIORef $ MkCookieJar []
  pure $ MkHttpClient jar store_cookie follow_redirect manager

||| Create a new HTTP client with default configuration.
||| This would also verify the website's TLS certificate with the system's trusted CAs.
||| max_total_connection = 25
||| max_per_site_connection = 5
||| store_cookie = True
||| follow_redirect = True
export
new_client_default : HasIO io => io (HttpClient e)
new_client_default = do
  Right certs <- liftIO get_system_trusted_certs
  | Left err => assert_total $ idris_crash "error when trying to get system certificats, please report this issue."
  new_client (certificate_check certs) 25 5 True True

replace : Eq a => List (a, b) -> List (a, b) -> List (a, b)
replace original [] = original
replace original ((k, v) :: xs) = replace (loop [] original k v) xs where
  loop : List (a, b) -> List (a, b) -> a -> b -> List (a, b)
  loop acc [] k v = acc
  loop acc ((k', v') :: xs) k v = if k' == k then (k, v) :: (acc <+> xs) else loop ((k', v') :: acc) xs k v

add_if_not_exist : Eq a => (a, b) -> List (a, b) -> List (a, b)
add_if_not_exist (k, v) headers = if any (\(k', v') => k' == k) headers then headers else (k, v) :: headers

unwrap : Functor f => Stream f (EitherT e IO) a -> Stream f IO (Either e a)
unwrap = fold (Return . Right) (go . runEitherT) (\x => Step x) where
  go : IO (Either e (Stream f IO (Either e a))) -> ?
  go stream = case !(lift stream) of
    Left err => Return (Left err)
    Right stream => stream

wrap : (Functor f, MonadError e m) => HasIO m => Stream f IO (Either e ()) -> Stream f m ()
wrap = fold (\case Right a => Return a; Left a => Effect $ throwError a) (Effect . delay . liftIO) (\x => Step x)

||| Send a HTTP request, returns a `HttpResponse` containing the status code and headers,
||| and also a stream of the content body from the response.
||| Arguments:
|||
||| @ client the HTTP client
||| @ method the HTTP method, e.g. GET / POST
||| @ url the URL to of the website to be connected to
||| @ headers the HTTP headers, represented as a list of (key, value)
||| @ length the length of the content to be sent
||| @ input the stream of bytes to be sent, which should be at least `length` bytes
export
request' : {e,m : _} -> MonadError (HttpError e) m => HasIO m => HttpClient e -> Method -> URL -> List (String, String)
  -> (length: Nat)
  -> (input : Stream (Of Bits8) (EitherT e IO) ())
  -> m (HttpResponse, Stream (Of Bits8) m ())
request' client method url headers payload_size payload = do
  let Just protocol = protocol_from_str url.protocol
  | Nothing => throwError (UnknownProtocol url.protocol)

  cookies_jar <- liftIO $ readIORef client.cookie_jar

  let headers_with_missing_info =
        add_if_not_exist ("Host", hostname_string url.host) .
        add_if_not_exist ("User-Agent", "idris2-http") .
        add_if_not_exist ("Content-Length", show payload_size) .
        add_if_not_exist ("Cookie", join "; " $ map serialize_cookie_no_attr cookies_jar.cookies) $ headers

  let message = MkRawHttpMessage method (show url.path <+> url.extensions) headers_with_missing_info
  Right (response, content) <- liftIO $ start_request {m=IO} client.pool_manager protocol message (unwrap payload)
  | Left err => throwError err

  when client.store_cookie $ do
    let cookies = lookup_headers response.headers SetCookie
    liftIO $ modifyIORef client.cookie_jar (\og => foldl add_cookie og cookies)

  if (client.follow_redirect && (Redirection == status_code_class response.status_code.snd))
    then do
      let Just location = lookup_header response.headers Location
      | Nothing => throwError (MissingHeader "Location")

      -- discard responded content to make way for another request
      liftIO $ consume content
      request' client method (add url location) headers payload_size payload
    else
      pure $ map wrap (response, content)

public export
interface Bytestream (a : Type) where
  to_stream : Monad m => a -> (Nat, Stream (Of Bits8) m ())

export
Bytestream () where
  to_stream () = (0, pure ())

export
Bytestream (List Bits8) where
  to_stream list = (length list, fromList_ list)

export
Bytestream String where
  to_stream = to_stream . utf8_unpack


||| Send a HTTP request, returns a `HttpResponse` containing the status code and headers,
||| and also a stream of the content body from the response.
||| Arguments:
|||
||| @ client the HTTP client
||| @ method the HTTP method, e.g. GET / POST
||| @ url the URL to of the website to be connected to
||| @ headers the HTTP headers, represented as a list of (key, value)
||| @ payload the content payload of the request. Use () for empty content.
export
request : {e,m,a : _} -> MonadError (HttpError e) m => HasIO m => Bytestream a =>
          HttpClient e -> Method -> URL -> List (String, String) ->
          a ->
          m (HttpResponse, Stream (Of Bits8) m ())
request client method url headers payload =
  let (len, stream) = to_stream {m=EitherT e IO} payload
  in request' client method url headers len stream
