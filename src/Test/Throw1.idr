module Test.Throw1

import Control.Linear.LIO
import Network.HTTP.Error

throw1 : (HttpError -> IO ()) -> (HttpError -> L IO ())
throw1 throw error = liftIO1 (throw error)
