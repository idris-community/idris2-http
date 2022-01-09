module Network.HTTP.Certificate

import Network.HTTP.Certificate.Raw
import Data.String.Parser
import Network.TLS

||| Parse a list of PEM blobs into adt representation of certificate
parse_report_error : List Certificate -> List PEMBlob -> Either String (List Certificate)
parse_report_error acc [] = Right acc
parse_report_error acc (x@(MkPEMBlob "CERTIFICATE" content) :: xs) =
  case parse_certificate content of
    Right cert => parse_report_error (cert :: acc) xs
    Left err => Left $ "error: " <+> err <+> ", content:\n" <+> encode_pem_blob x
parse_report_error acc ((MkPEMBlob type _) :: xs) = Left $ "unknown PEM type: " <+> type 

certificate' : Either String (List Certificate)
certificate' = do
  (certs_bin, _) <- parse (many parse_pem_blob) certificates_txt
  parse_report_error [] certs_bin

public export
certificates : List Certificate
certificates = assert_total $ case certificate' of Right x => x; Left err => idris_crash err
