module AuthorizationTest

import Network.HTTP
import Control.Monad.Error.Either
import Data.List1
import Data.String
import Data.String.Base64

%default partial

export
test_base64_encoding_decoding : EitherT String IO ()
test_base64_encoding_decoding =
  let user           = "aladdin"
      passwd         = "opensesame"
      (_, basicauth) = applyBasicAuth user passwd
      basicauth'     = base64DecodeString $ last $ split ((==) ' ') basicauth
    in case basicauth' of
     Nothing          =>
       idris_crash "couldn't base64 decode string."
     Just basicauth'' =>
       let basicauth''' = fastPack $ map (\x => chr $ the Int (cast x)) basicauth''
           userpasswd = split ((==) ':') basicauth'''
           user'      = head userpasswd
           passwd'    = last userpasswd
         in case basicauth == "Basic YWxhZGRpbjpvcGVuc2VzYW1l" &&
                 user      == user'                            &&
                 passwd    == passwd' of
              True  =>
                pure ()
              False =>
                throwE "base64 encode/decode error: expected \{user} and \{user'}, \{passwd} and \{passwd'} to be equivalent."
