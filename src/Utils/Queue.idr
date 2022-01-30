module Utils.Queue

import Data.Seq.Unsized
import Data.IORef
import Data.List
import System.Concurrency

data QueueEvent a = Msg a | Chan (Channel a)

export
data Queue a = Q Mutex (IORef (Seq (QueueEvent a)))

export
mk_queue : HasIO io => io (Queue a)
mk_queue = liftIO $ pure $ Q !makeMutex !(newIORef empty)

||| receive a message, if empty, block until there is one
export
recv : HasIO io => Queue a -> io a
recv (Q mutex ref) = liftIO $ do
  mutexAcquire mutex
  queue <- readIORef ref
  case viewl queue of
    Just (Msg message, rest) => do
      writeIORef ref rest
      mutexRelease mutex
      pure message
    _ => do
      chan <- makeChannel
      writeIORef ref (snoc queue (Chan chan))
      mutexRelease mutex
      channelGet chan

||| receive a message, if empty, returns nothing
export
recv' : HasIO io => Queue a -> io (Maybe a)
recv' (Q mutex ref) = liftIO $ do
  mutexAcquire mutex
  queue <- readIORef ref
  case viewl queue of
    Just (Msg message, rest) => do
      writeIORef ref rest
      mutexRelease mutex
      pure $ Just message
    _ => do
      mutexRelease mutex
      pure Nothing

||| receive all the messages waiting to be processed
export
recv_all : HasIO io => Queue a -> io (List a)
recv_all (Q mutex ref) = liftIO $ do
  mutexAcquire mutex
  queue <- readIORef ref
  let (msgs, others) = spanBy (\case Msg msg => Just msg; _ => Nothing) (toList queue)
  writeIORef ref (fromList others)
  mutexRelease mutex
  pure msgs

||| send a message to one of the receiver
export
signal : HasIO io => Queue a -> a -> io ()
signal (Q mutex ref) msg = liftIO $ do
  mutexAcquire mutex
  queue <- readIORef ref
  case viewl queue of
    Just (Chan chan, rest) => do
      writeIORef ref rest
      mutexRelease mutex
      channelPut chan msg
    _ => do
      writeIORef ref (snoc queue (Msg msg))
      mutexRelease mutex

||| send a message to all the receivers
export
broadcast : HasIO io => Queue a -> a -> io ()
broadcast (Q mutex ref) msg = liftIO $ do
  mutexAcquire mutex
  queue <- readIORef ref
  writeIORef ref empty
  mutexRelease mutex
  let channels = mapMaybe (\case Chan chan => Just chan; _ => Nothing) (toList queue)
  traverse_ (flip channelPut msg) channels
