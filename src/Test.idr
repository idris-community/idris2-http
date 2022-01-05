module Test

import Data.String.Parser
import Data.String.Extra
import Generics.Derive
import Network.HTTP.Header
import Network.HTTP.Status
import Network.HTTP.Method
import Data.Mime.Apache

import Debug.Trace

%language ElabReflection

public export
record RawHttpMessage where
  constructor MkRawHttpMessage
  method : HttpMethod
  path : String
  headers : Headers

public export
record RawHttpResponse where
  constructor MkRawHttpResponse
  status_code : (n ** StatusCode n)
  status_name : String
  headers : Headers

%runElab derive "RawHttpMessage" [Generic, Meta, Show]
%runElab derive "RawHttpResponse" [Generic, Meta, Show]

serialize_http_message : RawHttpMessage -> String
serialize_http_message message =
  join "\r\n"
  $ [ http_method_to_string message.method <+> " " <+> message.path <+> " HTTP/1.1" ]
  <+> map (\(k ** v) => header_key_name k <+> ": " <+> header_write_value k v) message.headers
  <+> [ "", "" ]

serialize_http_response : RawHttpResponse -> String
serialize_http_response response =
  join "\r\n"
  $ [ "HTTP/1.1 " <+> show (response.status_code.fst) <+> response.status_name ]
  <+> map (\(k ** v) => header_key_name k <+> ": " <+> header_write_value k v) response.headers
  <+> [ "", "" ]

eol : Monad m => ParseT m ()
eol = (string "\r\n" <|> string "\n") $> ()

is_eol : Char -> Bool
is_eol '\r' = True
is_eol '\n' = True
is_eol _ = False

header : Parser (DPair Header $ \h => header_value_type h)
header = do
  key <- key_name_to_header <$> takeUntil ":"
  value <- takeWhile1 (not . is_eol)
  eol
  case header_parse_value key (ltrim value) of
    Just v => pure (key ** v)
    Nothing => fail $ "Cannot parse header: " <+> header_key_name key <+> ":" <+> value 

http_message_praser : Parser RawHttpMessage
http_message_praser = do
  method <- string_to_http_method . pack <$> some (satisfy isUpper)
  _ <- char ' '
  path <- takeUntil " "
  _ <- string "HTTP/1.1"
  eol
  headers <- many header
  eol
  pure (MkRawHttpMessage method path headers)

deserialize_http_message : String -> Either String RawHttpMessage
deserialize_http_message = map fst . parse http_message_praser

http_message_response : Parser RawHttpResponse
http_message_response = do
  _ <- string "HTTP/1.1 "
  status_code <- natural
  _ <- char ' '
  status_name <- takeWhile1 (not . is_eol)
  eol
  headers <- many header
  eol
  case is_status_code_number status_code of
    Yes ok => pure (MkRawHttpResponse (status_code ** nat_to_status_code status_code ok) status_name headers)
    No _ => fail $ "status code " <+> show status_code <+> " is outside of bound" 

deserialize_http_response : String -> Either String RawHttpResponse
deserialize_http_response = map fst . parse http_message_response

parse_mime_list : String -> Either String (List (Fin 1001, Mime))
parse_mime_list string = ?a $ map (split (';' ==)) $ split (',' ==) string
  where
    parse_mime : List String -> Either String (Fin 1001, Mime)
    parse_mime [x] = Right (1000, ?xy x)
    parse_mime [x,q] = Left "empty mime entry"
    parse_mime _ = Left "malformed mime entry"

--------- TEST
test : String
test =
  serialize_http_message $ MkRawHttpMessage GET "/"
    [ (Host ** "google.com")
    , (Accept ** "*/*")
    , (ContentLength ** 0)
    ]

test1 : String
test1 =
  """
  HTTP/1.1 200 OK
  Connection: close
  Content-Length: 3492
  Content-Type: text/html
  Date: Wed, 05 Jan 2022 09:01:01 GMT
  Last-Modified: Fri, 05 Nov 2021 00:48:27 GMT
  Server: OpenBSD httpd


  """
