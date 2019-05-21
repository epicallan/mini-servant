{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TypeApplications #-}
module Servant.API.Alternative ( (:<|>)(..) )   where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Servant.Server (HasServer (..), RouteResult (..), RoutingApplication)

infixr 7 :<|>
data (firstAlt :: Type) :<|> (secondAlt :: Type) = firstAlt :<|> secondAlt

instance (HasServer firstAlt, HasServer secondAlt) => HasServer (firstAlt :<|> secondAlt) where

  type Server (firstAlt :<|> secondAlt) = Server firstAlt :<|> Server secondAlt

  route :: Proxy (firstAlt :<|> secondAlt) -> Server firstAlt :<|> Server secondAlt -> RoutingApplication
  route _ (firstHandler :<|> secondHandler) request = do
    result <- route (Proxy @firstAlt) firstHandler request
    case routeResult result of
      Right _ -> return result
      Left _  -> route (Proxy @secondAlt) secondHandler request
