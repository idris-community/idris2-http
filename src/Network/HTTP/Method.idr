module Network.HTTP.Method

import Generics.Derive

%language ElabReflection

public export
data Method : Type where
  GET : Method
  HEAD : Method
  POST : Method
  PUT : Method
  DELETE : Method
  CONNECT : Method
  OPTIONS : Method
  PATCH : Method
  Custom : String -> Method

%runElab derive "Method" [Generic, Meta, Eq, DecEq, Ord, Show]

export
http_method_to_string : Method -> String
http_method_to_string (Custom x) = toUpper x
http_method_to_string x = show x

export
string_to_http_method : String -> Method
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

export
FromString Method where
  fromString = string_to_http_method
