{-# LANGUAGE OverloadedStrings #-}

module ResponseError (mkServerError, ResponseError, ToServerError (..), NotFound, error404) where

import Servant (ServerError(..), err404)
import Data.Aeson (ToJSON, encode, toJSON, object, (.=), Value)
import Network.HTTP.Types (hContentType)

data ToJSON a => ResponseError a = ResponseError
  { _code :: !String, _data :: !(Maybe a) } deriving (Eq, Show)

instance ToJSON a => ToJSON (ResponseError a) where
  toJSON (ResponseError msg err) = object $ [ "code" .= msg] <> dataField err
    where
      dataField (Just err) = [ "data" .= toJSON err ]
      dataField Nothing = []

class ToServerError a where
  toErrorCode :: a -> String
  toErrorBody :: a -> Maybe Value
  toBaseError :: a -> ServerError

-- puts ToJSON body in a Servant ServerError
mkServerError :: ToServerError a => a -> ServerError
mkServerError err = (toBaseError err)
  { errBody = (encode . toJSON) $ ResponseError (toErrorCode err) (toErrorBody err)
  , errHeaders = [(hContentType, "application/json")]
  }

data NotFound = NotFound

instance ToServerError NotFound where
  toErrorCode NotFound = "NOT_FOUND"
  toErrorBody NotFound = Nothing
  toBaseError NotFound = err404

error404 :: ServerError
error404 = mkServerError NotFound
