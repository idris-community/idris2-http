module Network.HTTP.Authorization

import Data.String.Base64
import Utils.String

private
buildBasicAuth :  String -- username
               -> String -- password
               -> (String, String)
buildBasicAuth user passwd =
  let userandpasswd  = user ++ ":" ++ passwd
      userandpasswd' = utf8_unpack userandpasswd
    in ("Authorization", "Basic " ++ base64EncodeString userandpasswd')

public export
applyBasicAuth :  String -- username
               -> String -- password
               -> (String, String)
applyBasicAuth user passwd =
  buildBasicAuth user passwd

private
buildBearerAuth : String -- token
                -> (String, String)
buildBearerAuth token =
  ("Authorization", "Bearer " ++ token)

public export
applyBearerAuth :  String -- token
                -> (String, String)
applyBearerAuth bearertoken =
  buildBearerAuth bearertoken
