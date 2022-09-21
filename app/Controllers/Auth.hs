{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Controllers.Auth where

import Servant.API
import Controllers.Auth.Signup

type AuthAPI = "auth" :> SignupAPI

auth = signup
