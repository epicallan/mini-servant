module Servant.Server.Internal
  ( module Servant.Server.Internal
  , module Servant.Server.Internal.Delayed
  , module Servant.Server.Internal.Router
  , module Servant.Server.Internal.RouteResult
  , module Servant.Server.Internal.RoutingApplication
  , module Servant.Server.Internal.ServerError
  , module Servant.Server.Internal.Handler
  )where

import Data.Aeson
import Data.String.Conversions
import GHC.TypeLits hiding (natVal)
import Network.HTTP.Types (Status (..), methodGet, methodHead)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Request (..), responseLBS)
import Servant.API
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.Handler
import Servant.Server.Internal.Router
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.RoutingApplication
import Servant.Server.Internal.ServerError
import Web.HttpApiData (FromHttpApiData, parseUrlPiece)

class HasServer api where
  type ServerT api (m :: Type -> Type) :: Type

  route
    :: Proxy api
    -> Delayed env (Server api)
    -> Router env


type Server api = ServerT api Handler

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m =  ServerT a m :<|> ServerT b m

  route
    :: Proxy (a :<|> b)
    -> Delayed env (Server a :<|> Server b)
    -> Router env
  route _ server = choice (route pa $ (\ (a :<|> _) -> a) <$> server)
                          (route pb $ (\ (_ :<|> b) -> b) <$> server)
    where
      pb = Proxy :: Proxy b
      pa = Proxy :: Proxy a

instance (KnownSymbol path, HasServer api) => HasServer (path :> api) where
  type ServerT (path :> api) m = ServerT api m

  route
    :: Proxy (path :> api)
    -> Delayed env (Server api)
    -> Router env
  route _ subServer = pathRouter textPath $ route (Proxy @api) subServer
    where
      textPath :: Text
      textPath = toText $ symbolVal (Proxy @path)

instance (HasServer api, FromHttpApiData ctyp) => HasServer (Capture symbol ctyp :> api) where
  type ServerT (Capture symbol ctyp :> api) m = ctyp -> ServerT api m

  route :: Proxy (Capture symbol ctyp :> api)
        -> Delayed env (ctyp -> Server api)
        -> Router env
  route _ d = CaptureRouter $ route (Proxy @api) $
    addCapture d
    (\txt -> case (parseUrlPiece txt :: Either Text ctyp) of
               Right a   -> return a
               Left text -> delayedFail err400{ errBody = cs text}
    )

methodRouter
  :: forall a env . ToJSON a
  => Method -> Status -> Delayed env (Handler a) -> Router env
methodRouter method status action = leafRouter route'
  where
    -- route' :: env -> Router env
    route' env request respond =
      runAction
        (action `addMethodCheck` methodCheck method request)
        env
        request
        respond
        $ \ output -> Route $ responseLBS status [] (encode output)

instance (ToJSON a, KnownNat status, ReflectMethod method) => HasServer (Verb method status a) where
  type ServerT (Verb method status a) m = m a

  route :: Proxy (Verb method status a)
        -> Delayed env (Handler a)
        -> Router env
  route _ = methodRouter method status
    where
      method :: Method
      method = reflectMethod (Proxy @method)

      status :: Status
      status = toEnum . fromIntegral $ natVal (Proxy :: Proxy status)

allowedMethodHead :: Method -> Request -> Bool
allowedMethodHead method request = method == methodGet && requestMethod request == methodHead

allowedMethod :: Method -> Request -> Bool
allowedMethod method request = allowedMethodHead method request || requestMethod request == method

methodCheck :: Method -> Request -> DelayedIO ()
methodCheck method request
  | allowedMethod method request = return ()
  | otherwise                    = delayedFail err405
