module Network.HTTP.Scheduler

import Network.HTTP.Message
import Network.HTTP.Error
import Network.HTTP.Protocol
import Network.HTTP.Header
import Utils.Streaming
import System.Concurrency
import Data.List1
import Data.Compress.Interface
import Data.Compress.GZip
import Data.Compress.ZLib

public export
record ScheduleResponse (e : Type) (m : Type -> Type) where
  constructor MkScheduleResponse
  raw_http_response : HttpResponse
  content : Channel (Either (HttpError e) (Maybe (List Bits8)))

public export
record ScheduleRequest (e : Type) (m : Type -> Type) where
  constructor MkScheduleRequest
  raw_http_message : RawHttpMessage
  content : Stream (Of Bits8) m (Either e ())
  response : Channel (Either (HttpError e) (ScheduleResponse e m))

public export
interface Scheduler (e : Type) (m : Type -> Type) (0 a : Type) where
  ||| Schedule a HTTP request
  schedule_request : a -> Protocol -> ScheduleRequest e m -> m ()
  ||| Evict all HTTP connections, returning scheduler to a clean state (and closing all resources)
  evict_all : a -> m ()

channel_to_stream : (HasIO m, Decompressor c) => c -> Channel (Either (HttpError e) (Maybe (List Bits8))) ->
                    Stream (Of Bits8) m (Either (HttpError e) ())
channel_to_stream decomp channel = do
  Right (Just content) <- liftIO $ channelGet channel
  | Right Nothing =>
      case done decomp of
        Right [] => pure (Right ())
        Right xs => pure (Left $ DecompressionError "\{show $ length xs} leftover bytes in decompression buffer")
        Left err => pure (Left $ DecompressionError err)
  | Left err => pure (Left err)
  let Right (content, decomp) = feed decomp content
  | Left err => pure (Left $ DecompressionError err)
  fromList_ content *> channel_to_stream decomp channel

decompressor : List ContentEncodingScheme -> DPair Type Decompressor
decompressor [ GZip ] = MkDPair GZipState %search
decompressor [ Deflate ] = MkDPair ZLibState %search
decompressor _ = MkDPair IdentityState %search

to_list : Maybe (List1 a) -> List a
to_list Nothing = []
to_list (Just (x ::: xs)) = x :: xs

public export
start_request : {m, e : _} -> (HasIO m, Scheduler e m scheduler) =>
          scheduler ->
          Protocol ->
          RawHttpMessage ->
          Stream (Of Bits8) m (Either e ()) ->
          m (Either (HttpError e) (HttpResponse, Stream (Of Bits8) m (Either (HttpError e) ())))
start_request scheduler protocol msg content = do
  mvar <- makeChannel
  schedule_request scheduler protocol $ MkScheduleRequest msg content mvar
  Right response <- channelGet mvar
  | Left err => pure $ Left err
  let (encoding ** wit) = decompressor $ to_list $ lookup_header response.raw_http_response.headers ContentEncoding
  pure $ Right (response.raw_http_response, channel_to_stream (init @{wit}) response.content)
