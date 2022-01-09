module Network.HTTP.Pool.ConnectionPool

import Network.HTTP.Error
import Network.HTTP.Scheduler
import Network.HTTP.Protocol
import Network.HTTP.Message
import Network.HTTP.Header
import Network.HTTP.URL
import Network.HTTP.Pool.Worker
import Network.HTTP.Pool.Common
import Network.HTTP.Certificate
import Network.TLS
import Network.TLS.Signature
import Network.TLS.Verify
import Network.Socket
import Data.IORef
import Data.Bits
import Data.List1
import System
import System.Random
import System.Future
import System.Concurrency
import System.Concurrency.BufferedChannel

mutual
  record Pool (e : Type) where
    constructor MkPool
    workers : IORef (List (Worker e))
    ||| Increase everytime there is a new worker
    counter : IORef Bits32
    scheduled : IORef (BufferedChannel (Event e))
    sender : Sender e
    fetcher : Fetcher e
    last_called : IORef Integer

  export
  record PoolManager (e : Type) where
    constructor MkPoolManager
    pools : IORef (List (Hostname, Pool e))
    max_per_site_connections : Nat
    max_total_connections : Nat
    certificate_checker : (String -> CertificateCheck IO)

  record Worker (e : Type) where
    constructor MkWorker
    idle : IORef Bool
    ||| Unique id of the pool for identification
    uuid : Bits64
    protocol : Protocol
    ||| Kept in case we need to close externally
    socket : Socket
    parent : Pool e

export
new_pool_manager' : HasIO io => Nat -> Nat -> (String -> CertificateCheck IO) -> io (PoolManager e)
new_pool_manager' max_per_site_connections max_total_connections cert_check = liftIO $ do
  ref <- newIORef []
  pure (MkPoolManager ref max_per_site_connections max_total_connections cert_check)

export
new_pool_manager : HasIO io => Bool -> io (PoolManager e)
new_pool_manager True = new_pool_manager' 5 25 (certificate_check certificates) 
new_pool_manager False = new_pool_manager' 5 25 (certificate_ignore_check) 

pool_new_worker_id : Pool e -> IO Bits64
pool_new_worker_id pool = do
  r <- the (IO Int32) randomIO
  modifyIORef pool.counter (+ 1)
  c <- readIORef pool.counter
  pure $ (shiftL (cast r) 32) .|. cast c

find_or_create_pool : RawHttpMessage -> PoolManager e -> IO (Either HttpError (Hostname, Pool e))
find_or_create_pool message manager = do
  pools <- readIORef manager.pools
  case lookup_header message.headers Host of
    Just host =>
      case lookup host pools of
        Just pool =>
          pure (Right (host, pool))
        Nothing => do
          workers <- newIORef []
          counter <- newIORef 0
          buffer <- makeBufferedChannel
          sender <- becomeSender buffer
          fetcher <- becomeReceiver Blocking buffer
          last_called <- time >>= newIORef
          let pool = MkPool workers counter buffer sender fetcher last_called
          modifyIORef manager.pools ((host, pool) ::)
          pure (Right (host, pool))
    Nothing => pure (Left UnknownHost)

single_pool_active_connections : Protocol -> Pool e -> IO Nat
single_pool_active_connections protocol pool = length . filter (\x => protocol == x.protocol) <$> readIORef pool.workers

export
total_active_connections : Protocol -> PoolManager e -> IO Nat
total_active_connections protocol manager = do
  pools <- readIORef manager.pools
  let pools = map snd pools
  foldlM (\a,b => (a+) <$> single_pool_active_connections protocol b) 0 pools

close_worker : Worker e -> IO ()
close_worker worker = do
  close worker.socket
  let f = \w => w.uuid == worker.uuid
  modifyIORef worker.parent.workers (filter f)

close_pool : {e : _} -> Pool e -> IO ()
close_pool pool = do
  buffer_reader <- becomeReceiver NonBlocking pool.scheduled
  remaining <- get_all_remaining_requests buffer_reader 0 []
  -- feed all awaiting requests with errors
  traverse_ (flip channelPut (Left $ Left ConnectionClosed) . response) remaining

  -- close all sockets
  workers <- readIORef pool.workers
  traverse_ close_worker workers

  -- spam kill events
  let kills = List.replicate (2 * length workers) (the (Event e) Kill)
  let (bc ** buffer) = pool.sender
  traverse_ {t=List} (buffer Signal bc) kills
  where
    get_all_remaining_requests : (dc : BufferedChannel (Event e) ** (NonBlockingReceiver (Event e)))
                                  -> Integer
                                  -> List (ScheduleRequest e IO)
                                  -> IO (List (ScheduleRequest e IO))
    get_all_remaining_requests (channel ** recv) tries acc =
      -- Try extra 5 times just in case
      if tries >= 5 then pure acc else do
        Just (Request x) <- recv channel
        | _ => get_all_remaining_requests (channel ** recv) (tries + 1) acc
        get_all_remaining_requests (channel ** recv) 0 (x :: acc)

has_idle_worker : Pool e -> IO Bool
has_idle_worker pool = readIORef pool.workers >>= loop where
  loop : List (Worker e) -> IO Bool
  loop [] = pure False
  loop (x :: xs) = readIORef x.idle >>= (\b => if b then pure True else loop xs)

pools_last_called : PoolManager e -> IO (List (Integer, Pool e))
pools_last_called manager = do
  pools <- readIORef manager.pools
  let pools = map snd pools
  flip traverse pools $ \pool => do
    t <- readIORef pool.last_called
    pure (t, pool)

spawn_worker : {e : _} -> Fetcher e -> (HttpError -> IO ()) -> (String -> CertificateCheck IO) -> Protocol -> Hostname -> Pool e -> IO ()
spawn_worker fetcher throw cert_check protocol hostname pool = do
  worker_id <- pool_new_worker_id pool
  Right sock <- socket AF_INET Stream 0
  | Left err => throw $ SocketError "error when creating socket: \{show err}"
  idle_ref <- newIORef True
  let worker = MkWorker idle_ref worker_id protocol sock pool
  modifyIORef pool.workers (worker ::)
  let hostname_str = hostname_string hostname
  let port = case hostname.port of Just p => p; Nothing => protocol_port_number protocol
  0 <- connect sock (Hostname hostname.domain) (cast port)
  | err => throw $ SocketError "unable to connect to \{hostname_str}: \{show err}"
  let closer = close_worker worker
  worker_handle sock idle_ref closer fetcher throw cert_check protocol hostname_str

min_by : (ty -> ty -> Ordering) -> List1 ty -> ty
min_by compare (x ::: xs) = loop x xs where
  loop : ty -> List ty -> ty
  loop x [] = x
  loop x (y :: ys) =
    case compare x y of
      LT => loop x ys
      _  => loop y ys

export
{e : _} -> Scheduler e IO (PoolManager e) where
  schedule_request manager protocol request = do
    let throw = \err => channelPut request.response (Left $ Left err)
    Right (host, pool) <- find_or_create_pool request.raw_http_message manager
    | Left err => throw err
  
    -- update last called
    time >>= writeIORef pool.last_called

    let (bc ** buffer) = pool.sender
    buffer Signal bc $ Request request

    {-
    1. if total connection is maxed and no local connection
      send kill to the last called pool
      spawn new thread
    2. else if local connection is not maxed and no idle
      spawn new thread
    -}

    local <- single_pool_active_connections protocol pool
    all <- total_active_connections protocol manager
    has_idle <- has_idle_worker pool

    let first_condition = (all >= manager.max_per_site_connections) && (local == Z)
    let second_condition = not first_condition && ((local < manager.max_per_site_connections) && not has_idle)

    when {f=IO} first_condition $ do
      pools_to_kill <- pools_last_called manager
      let Just pools_to_kill = fromList pools_to_kill
      | Nothing => pure () -- why is pool empty

      let pool_to_kill = min_by (\a,b => compare (fst a) (fst b)) pools_to_kill
      let (bc_to_kill ** buffer_to_kill) = pool.sender
      buffer_to_kill Signal bc_to_kill Kill
      _ <- forkIO $ spawn_worker pool.fetcher throw manager.certificate_checker protocol host pool
      pure ()

    when {f=IO} second_condition $ do
      _ <- forkIO $ spawn_worker pool.fetcher throw manager.certificate_checker protocol host pool
      pure ()

  evict_all manager = do
    pools <- readIORef manager.pools
    writeIORef manager.pools []
    traverse_ close_pool $ map snd pools
