module Network.HTTP.URL

import Data.String.Parser
import Generics.Derive
import Network.HTTP.Protocol

%language ElabReflection

public export
record Host where
  constructor MkHost
  domain : String
  port : Maybe Bits16

public export
record URL where
  constructor MkURL
  protocol : String
  host : Host
  path : String

%runElab derive "Host" [Generic, Meta, Eq, Show]
%runElab derive "URL" [Generic, Meta, Eq, Show]

parse_port_number : Parser Bits16
parse_port_number = do
  n <- natural
  if n < 65536 then pure (cast n) else fail "port number exceeds 65535"

parse_host : Parser Host
parse_host = do
  domain <- takeWhile (/= ':')
  port <- optional (char ':' *> parse_port_number)
  pure (MkHost domain port)

export
url : Parser URL
url = do
  protocol <- takeUntil "://"
  domain <- takeUntil "/"
  case parse parse_host domain of
    Right (d, _) => (MkURL protocol d . ("/" <+>)) <$> takeWhile (const True)
    Left err => fail err

export
parse_url : String -> Either String URL
parse_url = map fst . parse url . ltrim

export
url_port_number : URL -> Maybe Bits16
url_port_number url =
  url.host.port <|> case url.protocol of
    "http" => Just 80
    "https" => Just 443
    "ws" => Just 80
    "wss" => Just 443
    _ => Nothing
