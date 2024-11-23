module AuthorizationTest

import Network.HTTP
import Control.Monad.Error.Either
import Data.String.Base64

%default partial

export
test_base64_encoding_decoding : EitherT String IO ()
test_base64_encoding_decoding =
  let user           = "aladdin"
      passwd         = "opensesame"
      (_, basicauth) = applyBasicAuth user passwd
      basicauth'     = base64DecodeString basicauth
    in case basicauth' of
         Nothing          =>
           idris_crash "couldn't base64 decode string."
         Just basicauth'' =>
           let basicauth''' = fastPack $ map (\x => chr $ the Int (cast x)) basicauth'' 
               (user', passwd') = break ((==) ':') basicauth'''
             in case basicauth == "YWxhZGRpbjpvcGVuc2VzYW1l" &&
                     user      == user'                      &&
                     passwd    == passwd' of
                  True  =>
                    pure ()
                  False =>
                    throwE "base64 encode/decode error: expected \{user} and \{user'}, \{passwd} and \{passwd'} to be equivalent."
