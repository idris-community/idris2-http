module Network.HTTP.Protocol

import Data.String
import Generics.Derive

%language ElabReflection

public export
data Protocol : Type where
  HTTP : Protocol
  HTTPS : Protocol

%runElab derive "Protocol" [Generic, Meta, Eq, DecEq, Show]

public export
protocol_port_number : Protocol -> Bits16
protocol_port_number HTTP = 80
protocol_port_number HTTPS = 443

public export
protocol_from_str : String -> Maybe Protocol
protocol_from_str protocol =
  case toUpper protocol of
    "HTTP" => Just HTTP
    "HTTPS" => Just HTTPS
    _ => Nothing
