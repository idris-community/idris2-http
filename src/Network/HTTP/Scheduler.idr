module Network.HTTP.Scheduler

import Network.HTTP.Message
import Network.HTTP.Error
import Utils.Streaming
import System.Concurrency

public export
record ScheduleResponse (m : Type -> Type) where
  constructor MkScheduleResponse
  raw_http_response : RawHttpResponse
  content : Channel (Either HttpError (List Bits8))

public export
record ScheduleRequest (e : Type) (m : Type -> Type) where
  constructor MkScheduleRequest
  raw_http_message : RawHttpMessage
  content_length : Integer -- Integer for performance
  content : Stream (Of Bits8) m (Either e ())
  response : Channel (Either e (ScheduleResponse m))

public export
interface Scheduler (e : Type) (m : Type -> Type) (0 a : Type) where
  schedule_request : a -> ScheduleRequest e m -> m ()
  close : a -> m ()

channel_to_stream : HasIO m => Channel (Either HttpError (List Bits8)) -> Stream (Of Bits8) m (Either (Either HttpError e) ())
channel_to_stream channel = do
  Right content <- liftIO $ channelGet channel
  | Left err => pure (Left (Left err))
  fromList_ content *> channel_to_stream channel

public export
request : {m, e : _} -> (HasIO m, Scheduler e m scheduler) =>
          scheduler -> RawHttpMessage -> Integer -> Stream (Of Bits8) m (Either e ()) ->
          m (Either (Either HttpError e) (RawHttpResponse, Stream (Of Bits8) m (Either (Either HttpError e) ())))
request scheduler msg size content = do
  mvar <- makeChannel
  schedule_request scheduler $ MkScheduleRequest msg size content mvar 
  Right response <- channelGet mvar
  | Left err => pure $ Left (Right err)
  pure $ Right (response.raw_http_response, channel_to_stream response.content)
