{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Db
    ( CalendarDb (..)
    , UserT (..)
    , User
    , initDb
    , getUserById
    , getUserByEmail
    , createUser
    ) where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Query
import Db.Users
import Db.Connection
import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam.Postgres.Full (insertReturning)
import Database.Beam.Postgres.Conduit (runInsertReturning)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(runInsertReturningList), SqlSerial (SqlSerial))
import Data.Password.Bcrypt (hashPassword, mkPassword, PasswordHash (PasswordHash))

data CalendarDb f = CalendarDb
    { _calendarUsers :: f (TableEntity UserT)
    } deriving (Generic, Database Postgres)

calendarDb :: DatabaseSettings Postgres CalendarDb
calendarDb = defaultDbSettings

getUserById :: Connection -> Int32 -> IO (Maybe User)
getUserById conn userId = runBeamPostgres conn $
    runSelectReturningOne $ select $
        filter_ (\user -> _userId user ==. (val_ $ SqlSerial userId))
        (all_ (_calendarUsers calendarDb))

getUserByEmail :: Connection -> Text -> IO (Maybe User)
getUserByEmail conn userEmail = runBeamPostgres conn $
    runSelectReturningOne $ select $
        filter_ (\user -> _userEmail user ==. val_ userEmail)
        (all_ (_calendarUsers calendarDb))

createUser :: Connection -> Text -> Text -> IO User
createUser conn email password = do
    hashedPassword <- hashPassword (mkPassword password)
    res <- runBeamPostgres conn $ runInsertReturningList $
        insert (_calendarUsers calendarDb) $
        insertExpressions ([User default_ (val_ email) (val_ hashedPassword)] :: [UserT (QExpr Postgres a)])
    if length res == 1
       then pure $ res !! 0
       else (error "No user has been returned")


