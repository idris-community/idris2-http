module Network.HTTP.ConnectionPool

import Network.HTTP.Error
import Network.HTTP.Scheduler
import Network.HTTP.Protocol
import Network.HTTP.Message
import Network.HTTP.Header
import Network.Socket
import Data.IORef
import Data.Bits
import System
import System.Random
import System.Future
import System.Concurrency.BufferedChannel

mutual
  record Pool (e : Type) (m : Type -> Type) where
    constructor MkPool
    workers : IORef (List (Worker e m))
    ||| Increase everytime there is a new worker
    counter : IORef Bits32
    scheduled : IORef (BufferedChannel (ScheduleRequest e m))
  
  record PoolOfPool (e : Type) (m : Type -> Type) where
    constructor MkPoolOfPool
    pools : IORef (List (String, Pool e m))
    max_per_site_connections : Nat
    max_total_connections : Nat
  
  record Worker (e : Type) (m : Type -> Type) where
    constructor MkWorker
    ||| Unique id of the pool for identification
    uuid : Bits64
    protocol : Protocol
    ||| Kept in case we need to close externally
    socket : Socket
    parent : Pool e m

pool_new_worker_id : HasIO io => Pool e io -> io Bits64
pool_new_worker_id pool = do
  r <- the (io Int32) randomIO
  modifyIORef pool.counter (+ 1)
  c <- readIORef pool.counter
  pure $ (shiftL (cast r) 32) .|. cast c

find_or_create_pool : HasIO io => RawHttpMessage -> PoolOfPool e io -> io (Either HttpError (Pool e io))
find_or_create_pool message poolofpool = do
  pools <- readIORef poolofpool.pools
  case lookup_header message.headers Host of
    Just host =>
      case lookup host pools of
        Just pool =>
          pure (Right pool)
        Nothing => do
          workers <- newIORef []
          counter <- newIORef 0
          buffer <- liftIO $ makeBufferedChannel
          let pool = MkPool workers counter buffer
          modifyIORef poolofpool.pools ((host, pool) ::)
          pure (Right pool)
    Nothing => pure (Left UnknownHost)
