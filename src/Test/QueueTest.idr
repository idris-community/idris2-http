module Test.QueueTest

import Data.Compress.Utils.Queue
import System.Future
import System

spawn_worker : Queue String -> String -> IO ()
spawn_worker channel name = do
  thing <- recv channel
  putStrLn "\{name} got \{thing}"
  usleep 100
  spawn_worker channel name

send_stuff : Queue String -> IO ()
send_stuff queue = do
  t <- time
  signal queue "toby \{show t}"
  usleep 100
  send_stuff queue

brod_stuff : Queue String -> IO ()
brod_stuff queue = do
  t <- time
  broadcast queue "toby \{show t}"
  usleep 100
  send_stuff queue

main : (Queue String -> IO ()) -> IO ()
main f = do
  queue <- mk_queue
  putStrLn "spawning workers"
  _ <- forkIO $ spawn_worker queue "1"
  _ <- forkIO $ spawn_worker queue "2"
  _ <- forkIO $ spawn_worker queue "3"
  _ <- forkIO $ spawn_worker queue "4"

  putStrLn "signalling"
  f queue
