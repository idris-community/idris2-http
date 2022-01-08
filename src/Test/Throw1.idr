module Test.Throw1

import Control.Linear.LIO
import Network.HTTP.Error
import Utils.Streaming
import Data.Nat

awe : Integer -> IO (Either () (Of Integer Integer))
awe n = pure (Right ((n + 1) :> (n + 1)))

main : IO ()
main = do
  let ones = chunksOf 6 $ the (Stream (Of Integer) IO ()) $ unfold awe 0
  fold (\(x :> k) => printLn x *> k) (join) (\_ => pure ()) ones
  putStrLn "steam"
