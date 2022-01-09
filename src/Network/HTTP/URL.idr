module Network.HTTP.URL

import Data.String.Parser
import Generics.Derive
import Network.HTTP.Protocol
import Data.String
import Network.HTTP.Path
import Data.So

%language ElabReflection

public export
record Hostname where
  constructor MkHostname
  domain : String
  port : Maybe Bits16

public export
record URLCredential where
  constructor MkURLCredential
  username : String
  password : Maybe String

public export
record URL where
  constructor MkURL
  protocol : String
  credential : Maybe URLCredential
  host : Hostname
  path : Path
  extensions : Maybe String

%runElab derive "URLCredential" [Generic, Meta, Eq, Show]
%runElab derive "Hostname" [Generic, Meta, Eq, Show]
%runElab derive "URL" [Generic, Meta, Show]

parse_port_number : Parser Bits16
parse_port_number = do
  n <- natural
  _ <- eos
  if n < 65536 then pure (cast n) else fail "port number bigger than 65535"

parse_host : Parser Hostname
parse_host = do
  domain <- takeWhile (/= ':')
  port <- optional (char ':' *> (parse_port_number <|> pure 0))
  pure (MkHostname domain port)

parse_credential : Parser URLCredential
parse_credential = parse_username_password <|> parse_username where
  parse_username : Parser URLCredential
  parse_username = do
    username <- takeWhile (const True)
    pure (MkURLCredential username Nothing)
  parse_username_password : Parser URLCredential
  parse_username_password = do
    username <- takeUntil ":"
    password <- takeWhile (const True)
    pure (MkURLCredential username $ Just password)

export
parse_url : Parser URL
parse_url = do
  protocol <- takeUntil "://"
  credential <- optional (takeUntil "@")
  domain <- takeUntil "/" <|> takeWhile (const True)
  
  credential <- case parse parse_credential <$> credential of
    Just (Right (credential, _)) => pure $ Just credential
    Just (Left err) => fail err
    Nothing => pure Nothing

  case parse parse_host domain of
    Right (domain_and_port, _) => do
      path <- takeWhile (\c => (c /= '#') && (c /= '?'))
      extensions <- takeWhile (const True)
      let path = fromString ("/" <+> path)
      let extensions = if null extensions then Nothing else Just extensions
      pure $ MkURL protocol credential domain_and_port path extensions
    Left err => fail err

export
url : String -> Either String URL
url = map fst . parse parse_url . ltrim

public export
data URLProof : AsList m -> Type where
  IsHTTPURL : URLProof ('h' :: 't' :: 't' :: 'p' :: ':' :: '/' :: '/' :: xs)
  IsHTTPSURL : URLProof ('h' :: 't' :: 't' :: 'p' :: 's' :: ':' :: '/' :: '/' :: xs)

export
url' : (str : String) -> {auto ok : URLProof (asList str)} -> URL
url' string =
  case url string of
    Right x => x
    Left err => assert_total $ idris_crash err

export
add : URL -> String -> URL
add url' string =
  case url string of
    Right url'' => url''
    Left _ =>
      case break (\c => c == '#' || c == '?') string of
        (path, "") => { path := (url'.path <+> fromString path), extensions := Nothing } url'
        (path, extension) => { path := (url'.path <+> fromString path), extensions := (Just extension) } url'

export
parse_hostname : String -> Either String Hostname
parse_hostname = map fst . parse parse_host . trim

export
hostname_string : Hostname -> String
hostname_string host =
  host.domain <+> case host.port of Just x => ":\{show x}"; Nothing => ""

export
url_port_number : URL -> Maybe Bits16
url_port_number url = url.host.port <|> (protocol_port_number <$> protocol_from_str url.protocol)
