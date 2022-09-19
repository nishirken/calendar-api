{-# LANGUAGE RecordWildCards #-}

module Db.Connection where

import Database.Beam.Postgres
import Config

initDb:: Config -> IO Connection
initDb Config{..} = connect $ ConnectInfo dbHost dbPort dbUser "" dbName 
