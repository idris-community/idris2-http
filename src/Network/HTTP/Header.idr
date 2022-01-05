module Network.HTTP.Header

import Generics.Derive
import Utils.Num

%language ElabReflection

public export
data Header : Type where
  Host : Header
  ContentType : Header
  Accept : Header
  Cookie : Header
  ContentLength : Header
  Unknown : String -> Header

%runElab derive "Header" [Generic, Meta, Eq, DecEq, Ord, Show]

public export
header_value_type : Header -> Type
header_value_type ContentLength = Integer
header_value_type _ = String

export
header_key_name : Header -> String
header_key_name ContentType = "Content-Type"
header_key_name ContentLength = "Content-Length"
header_key_name (Unknown x) = x
header_key_name x = show x

export
key_name_to_header : String -> Header
key_name_to_header x =
  case toLower x of
    "host" => Host
    "content-type" => ContentType
    "accept" => Accept
    "cookie" => Cookie
    "content-length" => ContentLength
    x => Unknown x

export
header_parse_value : (header : Header) -> (String -> Maybe (header_value_type header))
header_parse_value Host = Just
header_parse_value ContentType = Just
header_parse_value Accept = Just
header_parse_value Cookie = Just
header_parse_value ContentLength = stringToNat' 10
header_parse_value (Unknown x) = Just

export
header_write_value : (header : Header) -> (header_value_type header -> String)
header_write_value Host = id
header_write_value ContentType = id
header_write_value Accept = id
header_write_value Cookie = id
header_write_value ContentLength = show
header_write_value (Unknown x) = id

export
Show (DPair Header $ \h => header_value_type h) where
  show (header ** value) = (header_key_name header) <+> ": " <+> (header_write_value header value)

public export
Headers : Type
Headers = List (DPair Header $ \h => header_value_type h)
