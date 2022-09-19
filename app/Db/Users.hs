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

module Db.Users where

import Database.Beam
import Database.Beam.Postgres
import Data.Text (Text)
import Data.Int (Int32)
import Data.Functor.Identity (Identity(Identity))

data UserT f
    = User
    { _userId :: Columnar f Int32
    , _userEmail     :: Columnar f Text
    , _userPassword  :: Columnar f Text }
    deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Beamable UserT

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Int32) deriving (Generic, Beamable)
   primaryKey = UserId . _userId

