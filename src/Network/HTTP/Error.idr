module Network.HTTP.Error

import Generics.Derive

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
  | OtherReason e

%runElab derive "HttpError" [Generic, Meta, Eq, Show]
