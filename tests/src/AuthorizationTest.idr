module AuthorizationTest

import Network.HTTP
import Control.Monad.Error.Either
import Data.List1
import Data.String
import Data.String.Base64
import Utils.String

%default partial

export
test_base64_encoding_decoding : EitherT String IO ()
test_base64_encoding_decoding = do
  let user = "aladdin"
  let passwd = "opensesame"
  let (_, basicauth) = applyBasicAuth user passwd
  
  basicauth' <- maybe (throwE "Couldn't base64 decode string.") pure $
                base64DecodeString $ last $ split ((==) ' ') basicauth
  
  basicauth'' <- maybe (throwE "Couldn't utf8 pack decoded string.") pure $
                  utf8_pack basicauth'

  let userpasswd = split ((==) ':') basicauth''
  let user' = head userpasswd
  let passwd' = last userpasswd

  if basicauth == "Basic YWxhZGRpbjpvcGVuc2VzYW1l" &&
     user == user' &&
     passwd == passwd'
    then pure ()
    else throwE $ "Base64 encode/decode error: expected {user} and {user'}, {passwd} and {passwd'} to be equivalent."
