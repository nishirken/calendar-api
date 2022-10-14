{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Logger where

import qualified Network.Wai as Wai
import Network.HTTP.Types.Status (Status (..))
import Network.HTTP.Types (RequestHeaders, Header, HeaderName)
import Data.Aeson (ToJSON, toJSON, Value, encode)
import Data.String.Interpolate (i)
import Data.CaseInsensitive (original, CI)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)

instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8

instance ToJSON HeaderName where
  toJSON = toJSON . original

logStdOut :: Wai.Request -> Status -> Maybe Integer -> IO ()
logStdOut req Status{..} _ = do
  let str = [i|#{method} #{statusCode} #{path} #{headers}|] :: String
  print str
  appendFile "requests.log" $ str <> "\n"
    where
      method = Wai.requestMethod req
      headers = encode $ Wai.requestHeaders req
      path = Wai.rawPathInfo req
