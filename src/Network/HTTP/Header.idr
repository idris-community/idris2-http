module Network.HTTP.Header

import Generics.Derive
import Utils.Num
import Data.Nat
import Data.Either
import Control.Monad.Error.Either
import Control.Monad.Trans
import Network.HTTP.Mime
import Network.HTTP.URL

import Network.HTTP.Utils

%language ElabReflection

public export
data Header : Type where
  Host : Header
  ContentType : Header
  Accept : Header
  Cookie : Header
  ContentLength : Header
  Connection : Header
  TransferEncoding : Header
  Location : Header
  Unknown : String -> Header

%runElab derive "Header" [Generic, Meta, Eq, DecEq, Ord, Show]

public export
data ConnectionAction : Type where
  KeepAlive : ConnectionAction
  Close : ConnectionAction

public export
data TransferEncodingScheme : Type where
  Chunked : TransferEncodingScheme
  UnknownScheme : String -> TransferEncodingScheme
  -- add others like gzip in the future

%runElab derive "ConnectionAction" [Generic, Meta, Eq, DecEq, Ord, Show]
%runElab derive "TransferEncodingScheme" [Generic, Meta, Eq, DecEq, Ord, Show]

public export
header_value_type : Header -> Type
header_value_type ContentLength = Integer
header_value_type Cookie = List (String, String)
header_value_type Host = Hostname
header_value_type Connection = ConnectionAction
header_value_type TransferEncoding = TransferEncodingScheme
header_value_type _ = String

export
header_key_name : Header -> String
header_key_name ContentType = "Content-Type"
header_key_name ContentLength = "Content-Length"
header_key_name TransferEncoding = "Transfer-Encoding"
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
header_parse_value TransferEncoding = \x =>
  case toLower $ trim x of
    "chunked" => Just Chunked
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
header_write_value TransferEncoding = \case UnknownScheme x => x; x => toLower $ show x

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
