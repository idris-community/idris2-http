module Network.HTTP.Error

import Generics.Derive

%language ElabReflection

public export
data HttpError : Type where
  ||| When the Host header is absent
  UnknownHost : HttpError
  UnknownProtocol : String -> HttpError
  ConnectionClosed : HttpError
  SocketError : String -> HttpError
  ContentLengthMismatch : (still_want : Integer) -> HttpError
  MissingHeader : String -> HttpError
  UnknownTransferEncoding : String -> HttpError

%runElab derive "HttpError" [Generic, Meta, Eq, Show]
