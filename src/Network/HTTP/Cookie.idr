module Network.HTTP.Cookie

import Utils.String
import Data.String.Extra
import Data.String
import Derive.Prelude

%language ElabReflection

public export
record Cookie where
  constructor MkCookie
  key : String
  value : String
  attributes : List (String, Maybe String)

public export
record CookieJar where
  constructor MkCookieJar
  cookies : List Cookie

%runElab derive "Cookie" [Eq, Ord, Show]
%runElab derive "CookieJar" [Eq, Ord, Show]

export
serialize_cookie_no_attr : Cookie -> String
serialize_cookie_no_attr cookie = "\{cookie.key}=\{cookie.value}"

export
serialize_cookie : Cookie -> String
serialize_cookie cookie = join "; " (serialize_cookie_no_attr cookie :: map serialize_attr cookie.attributes) where
  serialize_attr : (String, Maybe String) -> String
  serialize_attr (k, Nothing) = k
  serialize_attr (k, Just v) = "\{k}=\{v}"

export
same_key : Cookie -> Cookie -> Bool
same_key a b = a.key == b.key

export
add_cookie : CookieJar -> Cookie -> CookieJar
add_cookie jar cookie = MkCookieJar $ (cookie :: filter (not . same_key cookie) jar.cookies)

export
deserialize_cookie : String -> Maybe Cookie
deserialize_cookie string = do
  let (kv ::: attrs) = split (';' ==) string
  guard (isInfixOf "=" kv)
  let (k, v) = splitBy '=' kv
  pure $ MkCookie k v (map parse_attr attrs)
  where
    parse_attr : String -> (String, Maybe String)
    parse_attr attr = bimap ltrim (guard (isInfixOf "=" attr) $>) $ splitBy '=' attr
