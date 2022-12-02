module Network.HTTP.Error

import Derive.Prelude

%language ElabReflection

public export
data HttpError e
  = UnknownHost
  | UnknownProtocol String
  | ConnectionClosed
  | SocketError String
  | ContentLengthMismatch Integer
  | MissingHeader String
  | UnknownTransferEncoding String
  | DecompressionError String
  | OtherReason e

%runElab derive "HttpError" [Eq, Show]
