module Settings where

import Config (Config (..))
import Control.Exception (SomeException (SomeException))
import Data.ByteString.Lazy.UTF8 (fromString)
import Logger (logStdOut)
import Network.HTTP.Types.Status (Status (..), status500)
import Network.Wai
import Network.Wai.Handler.Warp

settings :: Config -> Settings
settings config =
  let response500 :: SomeException -> Response
      response500 err = responseLBS status500 [] (fromString $ "Something went wrong. " <> show err)
   in setPort (fromIntegral $ appPort config) $
        setOnExceptionResponse response500 $
          setLogger logStdOut defaultSettings
