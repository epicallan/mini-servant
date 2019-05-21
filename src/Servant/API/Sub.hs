{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TypeApplications #-}
module Servant.API.Sub ( (:>))  where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.Wai (Request (..), pathInfo)
import Servant.Server (HasServer (..), RouteMismatch (..), RouteResult (..), RoutingApplication)

infixr 9 :>
data (path :: k) :> (sublayout :: Type)

instance (HasServer sublayout, KnownSymbol path) => HasServer (path :> sublayout) where

  type Server (path :> sublayout) = Server sublayout

  route :: Proxy (path :> sublayout) -> Server sublayout -> RoutingApplication
  route _ action request = case pathInfo request of
    (x : xs) | x == toText (symbolVal (Proxy @path))
       -> route (Proxy @sublayout) action request{pathInfo = xs}

    _  -> return . RouteResult $ Left NotFound
