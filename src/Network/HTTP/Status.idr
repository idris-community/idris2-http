-- Based on https://www.iana.org/assignments/http-status-codes/http-status-codes.txt

module Network.HTTP.Status

import Data.Nat
import Generics.Derive

%language ElabReflection

public export
record StatusCodeNumber (n : Nat) where
  constructor Abv100Und600
  lte599 : LTE n 599
  gte100 : GTE n 100

export
is_status_code_number : (n : Nat) -> Dec (StatusCodeNumber n)
is_status_code_number n =
  case isLTE n 599 of
    Yes ok1 =>
      case isGTE n 100 of
        Yes ok2 => Yes (Abv100Und600 ok1 ok2)
        No nope => No (nope . gte100)
    No nope => No (nope . lte599)

public export
data StatusCodeClass : Type where
  Information : StatusCodeClass
  Successful : StatusCodeClass
  Redirection : StatusCodeClass
  ClientError : StatusCodeClass
  ServerError : StatusCodeClass

%runElab derive "StatusCodeClass" [Generic, Meta, Eq, Show]

public export
data StatusCode : Nat -> Type where
  Continue : StatusCode 100
  SwitchingProtocols : StatusCode 101
  Processing : StatusCode 102
  EarlyHints : StatusCode 103
  OK : StatusCode 200
  Created : StatusCode 201
  Accepted : StatusCode 202
  NonAuthoritativeInformation : StatusCode 203
  NoContent : StatusCode 204
  ResetContent : StatusCode 205
  PartialContent : StatusCode 206
  MultiStatus : StatusCode 207
  AlreadyReported : StatusCode 208
  IMUsed : StatusCode 226
  MultipleChoices : StatusCode 300
  MovedPermanently : StatusCode 301
  Found : StatusCode 302
  SeeOther : StatusCode 303
  NotModified : StatusCode 304
  UseProxy : StatusCode 305
  TemporaryRedirect : StatusCode 307
  PermanentRedirect : StatusCode 308
  Unauthorized : StatusCode 401
  PaymentRequired : StatusCode 402
  Forbidden : StatusCode 403
  NotFound : StatusCode 404
  MethodNotAllowed : StatusCode 405
  NotAcceptable : StatusCode 406
  ProxyAuthenticationRequired : StatusCode 407
  RequestTimeout : StatusCode 408
  Conflict : StatusCode 409
  Gone : StatusCode 410
  LengthRequired : StatusCode 411
  PreconditionFailed : StatusCode 412
  ContentTooLarge : StatusCode 413
  URITooLong : StatusCode 414
  UnsupportedMediaType : StatusCode 415
  RangeNotSatisfiable : StatusCode 416
  ExpectationFailed : StatusCode 417
  MisdirectedRequest : StatusCode 421
  UnprocessableContent : StatusCode 422
  Locked : StatusCode 423
  FailedDependency : StatusCode 424
  TooEarly : StatusCode 425
  UpgradeRequired : StatusCode 426
  PreconditionRequired : StatusCode 428
  TooManyRequests : StatusCode 429
  RequestHeaderFieldsTooLarge : StatusCode 431
  UnavailableForLegalReasons : StatusCode 451
  InternalServerError : StatusCode 500
  NotImplemented : StatusCode 501
  BadGateway : StatusCode 502
  ServiceUnavailable : StatusCode 503
  GatewayTimeout : StatusCode 504
  HTTPVersionNotSupported : StatusCode 505
  VariantAlsoNegotiates : StatusCode 506
  InsufficientStorage : StatusCode 507
  LoopDetected : StatusCode 508
  NetworkAuthenticationRequired : StatusCode 511
  UnknownStatusCode : (n : Nat) -> StatusCodeNumber n -> StatusCode n

export
Show (StatusCode n) where
  show Continue = "Continue"
  show SwitchingProtocols = "SwitchingProtocols"
  show Processing = "Processing"
  show EarlyHints = "EarlyHints"
  show OK = "OK"
  show Created = "Created"
  show Accepted = "Accepted"
  show NonAuthoritativeInformation = "NonAuthoritativeInformation"
  show NoContent = "NoContent"
  show ResetContent = "ResetContent"
  show PartialContent = "PartialContent"
  show MultiStatus = "MultiStatus"
  show AlreadyReported = "AlreadyReported"
  show IMUsed = "IMUsed"
  show MultipleChoices = "MultipleChoices"
  show MovedPermanently = "MovedPermanently"
  show Found = "Found"
  show SeeOther = "SeeOther"
  show NotModified = "NotModified"
  show UseProxy = "UseProxy"
  show TemporaryRedirect = "TemporaryRedirect"
  show PermanentRedirect = "PermanentRedirect"
  show Unauthorized = "Unauthorized"
  show PaymentRequired = "PaymentRequired"
  show Forbidden = "Forbidden"
  show NotFound = "NotFound"
  show MethodNotAllowed = "MethodNotAllowed"
  show NotAcceptable = "NotAcceptable"
  show ProxyAuthenticationRequired = "ProxyAuthenticationRequired"
  show RequestTimeout = "RequestTimeout"
  show Conflict = "Conflict"
  show Gone = "Gone"
  show LengthRequired = "LengthRequired"
  show PreconditionFailed = "PreconditionFailed"
  show ContentTooLarge = "ContentTooLarge"
  show URITooLong = "URITooLong"
  show UnsupportedMediaType = "UnsupportedMediaType"
  show RangeNotSatisfiable = "RangeNotSatisfiable"
  show ExpectationFailed = "ExpectationFailed"
  show MisdirectedRequest = "MisdirectedRequest"
  show UnprocessableContent = "UnprocessableContent"
  show Locked = "Locked"
  show FailedDependency = "FailedDependency"
  show TooEarly = "TooEarly"
  show UpgradeRequired = "UpgradeRequired"
  show PreconditionRequired = "PreconditionRequired"
  show TooManyRequests = "TooManyRequests"
  show RequestHeaderFieldsTooLarge = "RequestHeaderFieldsTooLarge"
  show UnavailableForLegalReasons = "UnavailableForLegalReasons"
  show InternalServerError = "InternalServerError"
  show NotImplemented = "NotImplemented"
  show BadGateway = "BadGateway"
  show ServiceUnavailable = "ServiceUnavailable"
  show GatewayTimeout = "GatewayTimeout"
  show HTTPVersionNotSupported = "HTTPVersionNotSupported"
  show VariantAlsoNegotiates = "VariantAlsoNegotiates"
  show InsufficientStorage = "InsufficientStorage"
  show LoopDetected = "LoopDetected"
  show NetworkAuthenticationRequired = "NetworkAuthenticationRequired"
  show (UnknownStatusCode n _) = "Unknown (" <+> show n <+> ")"

export
status_code_to_nat : {n : Nat} -> StatusCode n -> Nat
status_code_to_nat _ = n

export
nat_to_status_code : (n : Nat) -> (prf : StatusCodeNumber n) -> StatusCode n
nat_to_status_code 100 _ = Continue
nat_to_status_code 101 _ = SwitchingProtocols
nat_to_status_code 102 _ = Processing
nat_to_status_code 103 _ = EarlyHints
nat_to_status_code 200 _ = OK
nat_to_status_code 201 _ = Created
nat_to_status_code 202 _ = Accepted
nat_to_status_code 203 _ = NonAuthoritativeInformation
nat_to_status_code 204 _ = NoContent
nat_to_status_code 205 _ = ResetContent
nat_to_status_code 206 _ = PartialContent
nat_to_status_code 207 _ = MultiStatus
nat_to_status_code 208 _ = AlreadyReported
nat_to_status_code 226 _ = IMUsed
nat_to_status_code 300 _ = MultipleChoices
nat_to_status_code 301 _ = MovedPermanently
nat_to_status_code 302 _ = Found
nat_to_status_code 303 _ = SeeOther
nat_to_status_code 304 _ = NotModified
nat_to_status_code 305 _ = UseProxy
nat_to_status_code 307 _ = TemporaryRedirect
nat_to_status_code 308 _ = PermanentRedirect
nat_to_status_code 401 _ = Unauthorized
nat_to_status_code 402 _ = PaymentRequired
nat_to_status_code 403 _ = Forbidden
nat_to_status_code 404 _ = NotFound
nat_to_status_code 405 _ = MethodNotAllowed
nat_to_status_code 406 _ = NotAcceptable
nat_to_status_code 407 _ = ProxyAuthenticationRequired
nat_to_status_code 408 _ = RequestTimeout
nat_to_status_code 409 _ = Conflict
nat_to_status_code 410 _ = Gone
nat_to_status_code 411 _ = LengthRequired
nat_to_status_code 412 _ = PreconditionFailed
nat_to_status_code 413 _ = ContentTooLarge
nat_to_status_code 414 _ = URITooLong
nat_to_status_code 415 _ = UnsupportedMediaType
nat_to_status_code 416 _ = RangeNotSatisfiable
nat_to_status_code 417 _ = ExpectationFailed
nat_to_status_code 421 _ = MisdirectedRequest
nat_to_status_code 422 _ = UnprocessableContent
nat_to_status_code 423 _ = Locked
nat_to_status_code 424 _ = FailedDependency
nat_to_status_code 425 _ = TooEarly
nat_to_status_code 426 _ = UpgradeRequired
nat_to_status_code 428 _ = PreconditionRequired
nat_to_status_code 429 _ = TooManyRequests
nat_to_status_code 431 _ = RequestHeaderFieldsTooLarge
nat_to_status_code 451 _ = UnavailableForLegalReasons
nat_to_status_code 500 _ = InternalServerError
nat_to_status_code 501 _ = NotImplemented
nat_to_status_code 502 _ = BadGateway
nat_to_status_code 503 _ = ServiceUnavailable
nat_to_status_code 504 _ = GatewayTimeout
nat_to_status_code 505 _ = HTTPVersionNotSupported
nat_to_status_code 506 _ = VariantAlsoNegotiates
nat_to_status_code 507 _ = InsufficientStorage
nat_to_status_code 508 _ = LoopDetected
nat_to_status_code 511 _ = NetworkAuthenticationRequired
nat_to_status_code n prf = UnknownStatusCode n prf

export
status_code_class : {n : Nat} -> StatusCode n -> StatusCodeClass
status_code_class code =
  case (status_code_to_nat code) `div` 100 of
    1 => Information
    2 => Successful
    3 => Redirection
    4 => ClientError
    _ => ServerError
