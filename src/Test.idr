module Test

import Data.Nat 
import Network.HTTP
import Control.Monad.Error.Either
import Control.Monad.Error.Interface

ResultMonad : Type -> Type
ResultMonad = EitherT (HttpError ()) IO

getClient: IO (HttpClient ())
getClient = new_client certificate_ignore_check 1 1 False False

performRequest : HttpClient () -> ResultMonad (HttpResponse, Stream (Of Bits8) ResultMonad ())
performRequest client = 
  request client GET (url' "https://anglesharp.azurewebsites.net/Chunked") [("Authorization", "Bearer foo")] ()

silly_me = do
    client <- getClient
    Right (result, stream) <- runEitherT $ performRequest client
      | Left e => printLn $ "Error from calling url " ++ show e
    putStrLn $ show result
    Right (content, _) <- runEitherT $ toList stream
      | Left e => printLn $ "Error from reading response stream "  ++ show e   
    putStrLn $ show content
    putStrLn "Done"
    close client