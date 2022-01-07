module Network.HTTP.Error

public export
data HttpError : Type where
  ||| When the Host header is absent
  UnknownHost : HttpError
