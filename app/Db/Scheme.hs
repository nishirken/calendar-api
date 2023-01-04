{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Db.Scheme where

import Data.Int (Int32)
import Data.Password.Bcrypt (PasswordHash (PasswordHash), hashPassword, mkPassword)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList), SqlSerial (SqlSerial))
import Database.Beam.Postgres
import Database.Beam.Postgres.Conduit (runInsertReturning)
import Database.Beam.Postgres.Full (insertReturning)
import Database.Beam.Query
import Db.Users

newtype CalendarDb f = CalendarDb
  { _calendarUsers :: f (TableEntity UserT)
  }
  deriving (Generic, Database Postgres)

calendarDb :: DatabaseSettings Postgres CalendarDb
calendarDb = defaultDbSettings

getUserById :: Connection -> Int32 -> IO (Maybe User)
getUserById conn userId =
  runBeamPostgres conn $
    runSelectReturningOne $
      select $
        filter_
          (\user -> _userId user ==. val_ (SqlSerial userId))
          (all_ (_calendarUsers calendarDb))

getUserByEmail :: Connection -> Text -> IO (Maybe User)
getUserByEmail conn userEmail =
  runBeamPostgres conn $
    runSelectReturningOne $
      select $
        filter_
          (\user -> _userEmail user ==. val_ userEmail)
          (all_ (_calendarUsers calendarDb))

createUser :: Connection -> Text -> Text -> IO User
createUser conn email password = do
  hashedPassword <- hashPassword (mkPassword password)
  res <-
    runBeamPostgres conn $
      runInsertReturningList $
        insert (_calendarUsers calendarDb) $
          insertExpressions ([User default_ (val_ email) (val_ hashedPassword)] :: [UserT (QExpr Postgres a)])
  if length res == 1
    then pure $ head res
    else error "No user has been returned"
