{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DerivingStrategies               #-}
{-# LANGUAGE StandaloneDeriving               #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE UndecidableInstances               #-}
{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE DeriveGeneric               #-}

module Db
    ( CalendarDb (..)
    , UserT (..)
    , User
    , initDb
    , getUserById
    ) where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Query
import Db.Users
import Db.Connection
import Data.Int (Int32)

data CalendarDb f = CalendarDb
    { _calendarUsers :: f (TableEntity UserT)
    } deriving (Generic, Database Postgres)

calendarDb :: DatabaseSettings Postgres CalendarDb
calendarDb = defaultDbSettings

getUserById :: Connection -> Int32 -> IO (Maybe User)
getUserById conn userId = runBeamPostgres conn $ do
    user <- runSelectReturningOne $ select  $
        filter_ (\user -> _userId user ==. val_ userId)
        (all_ (_calendarUsers calendarDb))
    pure user

