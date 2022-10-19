{-# LANGUAGE RecordWildCards #-}

module Db.Connection where

import Config
import Database.Beam.Postgres

initDb :: Config -> IO Connection
initDb config@Config {..} = do
  print config
  connect $
    ConnectInfo
      { connectHost = dbHost,
        connectPort = dbPort,
        connectUser = dbUser,
        connectPassword = dbPassword,
        connectDatabase = dbName
      }
