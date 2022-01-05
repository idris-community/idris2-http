module Network.HTTP.Method

import Generics.Derive

%language ElabReflection

public export
data HttpMethod : Type where
  GET : HttpMethod
  HEAD : HttpMethod
  POST : HttpMethod
  PUT : HttpMethod
  DELETE : HttpMethod
  CONNECT : HttpMethod
  OPTIONS : HttpMethod
  PATCH : HttpMethod
  Custom : String -> HttpMethod

%runElab derive "HttpMethod" [Generic, Meta, Eq, DecEq, Ord, Show]

export
http_method_to_string : HttpMethod -> String
http_method_to_string (Custom x) = toUpper x
http_method_to_string x = show x

export
string_to_http_method : String -> HttpMethod
string_to_http_method x =
  case toUpper x of
    "GET" => GET
    "HEAD" => HEAD
    "POST" => POST
    "PUT" => PUT
    "DELETE" => DELETE
    "CONNECT" => CONNECT
    "OPTIONS" => OPTIONS
    "PATCH" => PATCH
    x => Custom x
