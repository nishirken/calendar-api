module Spec where

import Config (getConfig)
import SigninSpec (signinSpec)
import Test.Hspec (afterAll_, hspec)
import Utils (cleanUp, withEnv)

main :: IO ()
main = do
  config <- getConfig ".env"
  hspec $ afterAll_ (cleanUp config) $ do
    signinSpec config
