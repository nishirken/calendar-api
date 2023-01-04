{-# LANGUAGE RecordWildCards #-}

module Db.Connection where

import Config
import Database.Beam.Postgres

connectDb :: Config -> IO Connection
connectDb config@Config {..} = do
  connect $
    ConnectInfo
      { connectHost = dbHost,
        connectPort = dbPort,
        connectUser = dbUser,
        connectPassword = dbPassword,
        connectDatabase = dbName
      }
