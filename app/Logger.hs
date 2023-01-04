{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Logger where

import Data.Aeson (ToJSON, Value, encode, toJSON)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI, original)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (Header, HeaderName, RequestHeaders)
import Network.HTTP.Types.Status (Status (..))
import qualified Network.Wai as Wai

instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8

instance ToJSON HeaderName where
  toJSON = toJSON . original

logStdOut :: Wai.Request -> Status -> Maybe Integer -> IO ()
logStdOut req Status {..} _ = do
  let str = [i|#{method} #{statusCode} #{path} #{headers}|] :: String
  print str
  appendFile "requests.log" $ str <> "\n"
  where
    method = Wai.requestMethod req
    headers = encode $ Wai.requestHeaders req
    path = Wai.rawPathInfo req
