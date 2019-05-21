{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TypeApplications #-}
module Servant.API.Capture
         ( Capture
         , FromText(..)
         ) where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Network.Wai (pathInfo)
import Servant.API.Sub ((:>))
import Servant.Server (HasServer (..), RouteMismatch (..), RouteResult (..), RoutingApplication)

data Capture (capture :: k) (captureType :: Type)

instance (FromText captureType, HasServer sublayout)
  => HasServer (Capture capture captureType :> sublayout) where

  type Server (Capture capture captureType :> sublayout) = captureType -> Server sublayout

  route :: Proxy (Capture capture captureType :> sublayout)
        -> Server (Capture capture captureType :> sublayout)
        -> RoutingApplication
  route _px action request = case pathInfo request of
    (x : xs) -> case captured x of
                  Just value -> route (Proxy @sublayout) (action value) request{pathInfo = xs}
                  Nothing    -> return . RouteResult $ Left NotFound

    _  -> return . RouteResult $ Left NotFound

captured :: FromText a => Text -> Maybe a
captured = fromText

class FromText  a where
  fromText :: Text -> Maybe a

instance FromText Text where
  fromText = Just
