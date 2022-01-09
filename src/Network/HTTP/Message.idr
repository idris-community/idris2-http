module Network.HTTP.Message

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
RawHeaders : Type
RawHeaders = List (String, String)

public export
record RawHttpMessage where
  constructor MkRawHttpMessage
  method : Method
  path : String
  headers : RawHeaders

public export
record HttpResponse where
  constructor MkHttpResponse
  status_code : (n ** StatusCode n)
  status_name : String
  headers : RawHeaders

%runElab derive "RawHttpMessage" [Generic, Meta, Show]
%runElab derive "HttpResponse" [Generic, Meta, Show]

export
serialize_http_message : RawHttpMessage -> String
serialize_http_message message =
  join "\r\n"
  $ [ http_method_to_string message.method <+> " " <+> message.path <+> " HTTP/1.1" ]
  <+> map (\(k,v) => "\{k}: \{v}") message.headers
  <+> [ "", "" ]

export
serialize_http_response : HttpResponse -> String
serialize_http_response response =
  join "\r\n"
  $ [ "HTTP/1.1 " <+> show (response.status_code.fst) <+> response.status_name ]
  <+> map (\(k,v) => "\{k}: \{v}") response.headers
  <+> [ "", "" ]

eol : Monad m => ParseT m ()
eol = (string "\r\n" <|> string "\n") $> ()

is_eol : Char -> Bool
is_eol '\r' = True
is_eol '\n' = True
is_eol _ = False

header : Parser (String, String)
header = do
  key <- takeUntil ":"
  value <- takeWhile1 (not . is_eol)
  eol
  pure (key, (ltrim value))

export
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

export
deserialize_http_message : String -> Either String RawHttpMessage
deserialize_http_message = map fst . parse http_message_praser

export
http_message_response : Parser HttpResponse
http_message_response = do
  _ <- many eol
  _ <- string "HTTP/1.1 " <|> string "HTTP/1.0 "
  status_code <- natural
  _ <- char ' '
  status_name <- takeWhile1 (not . is_eol)
  eol
  headers <- many header
  case is_status_code_number status_code of
    Yes ok => pure (MkHttpResponse (status_code ** nat_to_status_code status_code ok) status_name headers)
    No _ => fail $ "status code " <+> show status_code <+> " is outside of bound" 

export
deserialize_http_response : String -> Either String HttpResponse
deserialize_http_response = map fst . parse http_message_response
