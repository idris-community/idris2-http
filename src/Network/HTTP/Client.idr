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
import Network.TLS.Signature
import Utils.Streaming
import Utils.String
import Utils.Bytes
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

export
close : {e : _} -> HasIO io => HttpClient e -> io ()
close client = liftIO $ evict_all {m=IO,e=e} $ client.pool_manager

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

public export
ResponseHeadersAndBody : Type -> Type
ResponseHeadersAndBody e = Either (HttpError e) (HttpResponse, Stream (Of Bits8) IO (Either (HttpError e) ()))

replace : Eq a => List (a, b) -> List (a, b) -> List (a, b)
replace original [] = original
replace original ((k, v) :: xs) = replace (loop [] original k v) xs where
  loop : List (a, b) -> List (a, b) -> a -> b -> List (a, b)
  loop acc [] k v = acc
  loop acc ((k', v') :: xs) k v = if k' == k then (k, v) :: (acc <+> xs) else loop ((k', v') :: acc) xs k v

add_if_not_exist : Eq a => (a, b) -> List (a, b) -> List (a, b)
add_if_not_exist (k, v) headers = if any (\(k', v') => k' == k) headers then headers else (k, v) :: headers

export
request' : {e : _} -> HttpClient e -> Method -> URL -> List (String, String)
  -> (length: Nat)
  -> (input : Stream (Of Bits8) IO (Either e ()))
  -> IO (ResponseHeadersAndBody e)
request' client method url headers payload_size payload = do
  let Just protocol = protocol_from_str url.protocol
  | Nothing => pure $ Left (UnknownProtocol url.protocol)

  cookies_jar <- readIORef client.cookie_jar

  let headers_with_missing_info =
        add_if_not_exist ("Host", hostname_string url.host) .
        add_if_not_exist ("User-Agent", "idris2-http") .
        add_if_not_exist ("Content-Length", show payload_size) .
        add_if_not_exist ("Cookie", join "; " $ map serialize_cookie_no_attr cookies_jar.cookies) $ headers

  let message = MkRawHttpMessage method (show url.path <+> url.extensions) headers_with_missing_info
  Right (response, content) <- start_request {m=IO} client.pool_manager protocol message payload
  | Left err => pure $ Left err

  when client.store_cookie $ do
    let cookies = lookup_headers response.headers SetCookie
    modifyIORef client.cookie_jar (\og => foldl add_cookie og cookies)

  if (client.follow_redirect && (Redirection == status_code_class response.status_code.snd))
    then do
      let Just location = lookup_header response.headers Location
      | Nothing => pure (Left $ MissingHeader "Location")

      -- discard responded content to make way for another request
      consume content
      request' client method (add url location) headers payload_size payload
    else
      pure $ Right (response, content)

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

export
request : {e,a : _} -> Bytestream a => HttpClient e -> Method -> URL -> List (String, String) -> a -> IO (ResponseHeadersAndBody e)
request  client method url headers payload =
  let (len, stream) = to_stream payload
  in request' client method url headers len (map Right stream)
