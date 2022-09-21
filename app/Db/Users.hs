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
{-# LANGUAGE UndecidableInstances #-}

module Db.Users where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Data.Text (Text, unpack)
import Data.Int (Int32)
import Data.Functor.Identity (Identity(Identity))
import Data.Password.Bcrypt (PasswordHash(..), Bcrypt)
import Database.Beam.Backend (autoSqlValueSyntax, HasSqlValueSyntax (sqlValueSyntax))
import Database.Beam.Backend.SQL.AST (Value(Value))

data UserT f
    = User
    { _userId :: Columnar f (SqlSerial Int32)
    , _userEmail     :: Columnar f Text
    , _userPassword  :: Columnar f UserPassword }
    deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity
type UserPassword = PasswordHash Bcrypt

deriving instance Show User
deriving instance Eq User

instance Beamable UserT

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be UserPassword where
  sqlValueSyntax password = sqlValueSyntax $ unPasswordHash password

instance FromBackendRow Postgres UserPassword where
  fromBackendRow = PasswordHash <$> fromBackendRow

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f (SqlSerial Int32)) deriving (Generic, Beamable)
   primaryKey = UserId . _userId

