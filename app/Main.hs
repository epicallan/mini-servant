{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
module Main (main)  where

import Data.Aeson
import Data.Proxy (Proxy)
import Network.Wai
import Network.Wai.Handler.Warp

import Servant

-- * Example

-- | A greet message data type
newtype Greet = Greet { msg :: Text }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- API specification
type TestApi =
      "name" :> Capture "name" Text :> Get Greet
 :<|> "hello" :> Get Greet

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'ExceptT' monad.
server :: Server TestApi
server = helloH :<|> greetH
  where
    helloH name = return $ Greet { msg = name }
    greetH = return $ Greet { msg = "hello" }


-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: Port -> IO ()
runTestServer port = run port test

-- Put this all to work!
main :: IO ()
main = do
  let port = 8001
  putTextLn $ "running server on " <> show port
  runTestServer port
