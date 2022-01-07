module Test.Channel

import System.Concurrency.BufferedChannel
import System.Concurrency
import System.Future
import System

spawn_worker : BufferedChannel String -> BlockingReceiver String -> String -> IO ()
spawn_worker channel receiver name = do
  thing <- receiver channel
  putStrLn "\{name} got \{thing}"
  sleep 1
  spawn_worker channel receiver name

send_stuff : DPair (BufferedChannel String) (\dc => SendEffect -> SenderFunc String) -> IO ()
send_stuff (bc ** buffer) = do
  t <- time
  putStrLn "send"
  buffer Signal bc "toby \{show t}"
  usleep 100000
  send_stuff (bc ** buffer)

main : IO ()
main = do
  ref <- makeBufferedChannel
  (channel ** func) <- becomeReceiver Blocking ref
  putStrLn "spawning workers"
  _ <- forkIO $ spawn_worker channel func "1"
  _ <- forkIO $ spawn_worker channel func "2"
  _ <- forkIO $ spawn_worker channel func "3"
  _ <- forkIO $ spawn_worker channel func "4"

  putStrLn "signalling"
  bcbuffer <- becomeSender ref
  send_stuff bcbuffer
