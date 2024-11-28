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
import Data.String
import System.Concurrency
import Network.HTTP.Status

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
  putStrLn "Asking for data"
  result <- liftIO $ channelGet channel
  putStrLn "Asked for data"
  case result of
    Right (Just content) => do
      putStrLn $ "got " ++ show content
      let Right (decompressed, newDecomp) = feed decomp content
        | Left err => pure (Left $ DecompressionError err)
      fromList_ decompressed *> channel_to_stream newDecomp channel
    Right Nothing => do
      putStrLn "Got nothing"
      case done decomp of
        Right [] => pure (Right ())
        Right xs => pure (Left $ DecompressionError "\{show $ length xs} leftover bytes in decompression buffer")
        Left err => pure (Left $ DecompressionError err)
    Left err => pure (Left err)


decompressor : List ContentEncodingScheme -> DPair Type Decompressor
decompressor [ GZip ] = MkDPair GZipState %search
decompressor [ Deflate ] = MkDPair ZLibState %search
decompressor _ = MkDPair IdentityState %search

to_list : Maybe (List1 a) -> List a
to_list Nothing = []
to_list (Just (x ::: xs)) = x :: xs


accumulateResponse2 : (HasIO m, Scheme a) => Channel a -> List a -> m (Either (HttpError e) (HttpResponse, List a))
accumulateResponse2 chan acc = do
  maybeChunk <- channelGetNonBlocking chan
  case maybeChunk of
    Nothing => 
      if null acc
        then pure $ Left $ SocketError "No data received"
        else pure $ Right (MkHttpResponse (MkDPair 200 OK) "" [], acc)
    Just chunk => accumulateResponse2 chan (acc ++ [chunk])


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
  result <- accumulateResponse2 mvar []
  case result of
    Left err => pure $ Left err
    Right (response, accumulatedContent) =>
      let (encoding ** wit) = decompressor $ to_list $ lookup_header response.headers ContentEncoding
      in pure $ Right (response, channel_to_stream (init @{wit}) (streamFromList accumulatedContent))

where
  streamFromList : List a -> Stream (Of a) m (Either (HttpError e) ())
  streamFromList [] = pure $ Right ()
  streamFromList (x :: xs) = x `cons` streamFromList xs
