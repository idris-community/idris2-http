module Network.HTTP.Scheduler

import Network.HTTP.Message
import Utils.Streaming
import System.Concurrency

record ScheduleResponse (m : Type -> Type) where
  constructor MkScheduleResponse
  raw_http_response : RawHttpResponse
  content : Channel (Maybe (List Bits8))

record ScheduleRequest (m : Type -> Type) where
  constructor MkScheduleRequest
  raw_http_message : RawHttpMessage
  content_length : Integer -- Integer for performance
  content : Stream (Of Bits8) m (Either String ())
  response : Channel (Either String (ScheduleResponse m))

public export
interface Scheduler (m : Type -> Type) (0 a : Type) where
  schedule_request : a -> ScheduleRequest m -> m ()
  close : a -> m ()

channel_to_stream : HasIO m => Channel (Maybe (List Bits8)) -> Stream (Of Bits8) m (Either String ())
channel_to_stream channel = do
  Just content <- liftIO $ channelGet channel
  | Nothing => pure (Right ())
  fromList_ content *> channel_to_stream channel

public export
request : {m : _} -> (HasIO m, Scheduler m scheduler) =>
          scheduler -> RawHttpMessage -> Integer -> Stream (Of Bits8) m (Either String ()) ->
          m (Either String (RawHttpResponse, Stream (Of Bits8) m (Either String ())))
request scheduler msg size content = do
  mvar <- makeChannel
  schedule_request scheduler $ MkScheduleRequest msg size content mvar 
  Right response <- channelGet mvar
  | Left err => pure $ Left err
  pure $ Right (response.raw_http_response, channel_to_stream response.content)
