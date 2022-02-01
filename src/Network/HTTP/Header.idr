module Network.HTTP.Header

import Generics.Derive
import Utils.Num
import Data.Nat
import Data.Either
import Control.Monad.Error.Either
import Control.Monad.Trans
import Network.HTTP.URL
import Network.HTTP.Cookie
import Utils.String

%language ElabReflection

public export
data Header
  = Host
  | ContentType
  | Accept
  | Cookie
  | SetCookie
  | ContentLength
  | Connection
  | TransferEncoding
  | ContentEncoding
  | Location
  | Unknown String

%runElab derive "Header" [Generic, Meta, Eq, DecEq, Ord, Show]

public export
data ConnectionAction : Type where
  KeepAlive : ConnectionAction
  Close : ConnectionAction

public export
data TransferEncodingScheme : Type where
  Chunked : TransferEncodingScheme
  Identity : TransferEncodingScheme

public export
data ContentEncodingScheme : Type where
  GZip : ContentEncodingScheme
  Deflate : ContentEncodingScheme

%runElab derive "ConnectionAction" [Generic, Meta, Eq, DecEq, Ord, Show]
%runElab derive "TransferEncodingScheme" [Generic, Meta, Eq, DecEq, Ord, Show]
%runElab derive "ContentEncodingScheme" [Generic, Meta, Eq, DecEq, Ord, Show]

public export
header_value_type : Header -> Type
header_value_type ContentLength = Integer
header_value_type Cookie = List (String, String)
header_value_type Host = Hostname
header_value_type Connection = ConnectionAction
header_value_type TransferEncoding = List1 TransferEncodingScheme
header_value_type ContentEncoding = List1 ContentEncodingScheme
header_value_type SetCookie = Cookie
header_value_type _ = String

export
header_key_name : Header -> String
header_key_name ContentType = "Content-Type"
header_key_name ContentLength = "Content-Length"
header_key_name TransferEncoding = "Transfer-Encoding"
header_key_name ContentEncoding = "Content-Encoding"
header_key_name SetCookie = "Set-Cookie"
header_key_name (Unknown x) = x
header_key_name x = show x

public export
key_name_to_header : String -> Header
key_name_to_header x =
  case toLower x of
    "host" => Host
    "content-type" => ContentType
    "accept" => Accept
    "cookie" => Cookie
    "content-length" => ContentLength
    "connection" => Connection
    "transfer-encoding" => TransferEncoding
    "content-encoding" => ContentEncoding
    "set-cookie" => SetCookie
    x => Unknown x

export
FromString Header where
  fromString = key_name_to_header

export
header_parse_value : (header : Header) -> (String -> Maybe (header_value_type header))
header_parse_value Host = getRight . parse_hostname
header_parse_value ContentType = Just
header_parse_value Accept = Just
header_parse_value Location = Just
header_parse_value ContentLength = stringToNat' 10
header_parse_value (Unknown x) = Just
header_parse_value Cookie = Just . map (splitBy '=' . ltrim) . forget . split (';' ==)
header_parse_value Connection = (\case "keep-alive" => Just KeepAlive; "close" => Just Close; _ => Nothing) . toLower . trim
header_parse_value SetCookie = deserialize_cookie
header_parse_value TransferEncoding = traverse parse_transfer_encoding . split (',' ==) where
  parse_transfer_encoding : String -> Maybe TransferEncodingScheme
  parse_transfer_encoding x =
    case toLower $ trim x of
      "chunked" => Just Chunked
      "identity" => Just Identity
      _ => Nothing
header_parse_value ContentEncoding = traverse parse_content_encoding . split (',' ==) where
  parse_content_encoding : String -> Maybe ContentEncodingScheme
  parse_content_encoding x =
    case toLower $ trim x of
      "gzip" => Just GZip
      "deflate" => Just Deflate
      _ => Nothing

export
header_write_value : (header : Header) -> (header_value_type header -> String)
header_write_value Host = hostname_string
header_write_value ContentType = id
header_write_value Accept = id
header_write_value Location = id
header_write_value Cookie = join "; " . map (\(a,b) => "\{a}=\{b}")
header_write_value ContentLength = show
header_write_value (Unknown x) = id
header_write_value Connection = \case KeepAlive => "keep-alive"; Close => "close"
header_write_value SetCookie = serialize_cookie
header_write_value TransferEncoding = join ", " . map (toLower . show)
header_write_value ContentEncoding = join ", " . map (toLower . show)

export
Show (DPair Header $ \h => header_value_type h) where
  show (header ** value) = (header_key_name header) <+> ": " <+> (header_write_value header value)

public export
Headers : Type
Headers = List (DPair Header $ \h => header_value_type h)

export
eq_ignore_case : String -> String -> Bool
eq_ignore_case a b = toUpper a == toUpper b

export
lookup_header : List (String, String) -> (header : Header) -> Maybe (header_value_type header)
lookup_header headers header = do
  raw_value <- lookupBy eq_ignore_case (header_key_name header) headers
  header_parse_value header raw_value

export
lookup_headers : List (String, String) -> (header : Header) -> List (header_value_type header)
lookup_headers headers header = mapMaybe go headers where
  go : (String, String) -> Maybe (header_value_type header)
  go (k, v) = do
    guard (eq_ignore_case k (header_key_name header))
    header_parse_value header v
