{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Config (Config (..), getConfig)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException (SomeException), catches, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Controllers
import Data.Aeson (toJSON)
import Data.ByteString.Lazy.UTF8 (fromString)
import Database.PostgreSQL.Simple (Connection)
import Db (initDb)
import GHC.Generics
import Logger (logStdOut)
import Network.HTTP.Types.Status (Status (..), status500)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy (..), simpleCors)
import Servant
import Servant.API
import Network.HTTP.Types (methodGet, methodPost, methodPut, methodDelete)

api' :: Proxy API
api' = Proxy

app :: Config -> Connection -> Wai.Application
app config connection = serveWithContext api' (genAuthServerContext config) $ api config connection

main :: IO ()
main = do
  config <- getConfig
  let port = appPort config
  print $ "Starting server at " ++ show port
  connection <- initDb config
  let response500 :: SomeException -> Wai.Response
      response500 err = Wai.responseLBS status500 [] (fromString $ "Something went wrong. " <> show err)
      settings =
        Warp.setPort (fromIntegral port) $
          Warp.setOnExceptionResponse response500 $
            Warp.setLogger logStdOut $
              Warp.defaultSettings
      corsMiddleware = cors $ \_ -> Just $ simpleCorsResourcePolicy
        { corsMethods = [ methodGet, methodPost, methodPut, methodDelete ]
        , corsOrigins = Just (["http://127.0.0.1:8080"], True)
        , corsRequestHeaders = ["Content-Type"]
        }
  Warp.runSettings settings $ corsMiddleware $ app config connection
