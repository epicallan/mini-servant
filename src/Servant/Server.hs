module Servant.Server
       ( HasServer(..)
       , RouteResult (..)
       , RouteMismatch (..)
       , ServantError (..)
       , RoutingApplication
       , Handler
       , serve
       ) where
import Control.Monad.Cont (ContT (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Network.HTTP.Types.Status (badRequest400)
import Network.Wai (Application, Request, Response, ResponseReceived, responseLBS)

import qualified Data.ByteString.Lazy as LB

data RouteMismatch =
    NotFound    -- ^ the usual "not found" error
  | WrongMethod -- ^ a more informative "you just got the HTTP method wrong" error
  | InvalidBody -- ^ an even more informative "your json request body wasn't valid" error
  deriving (Eq, Show)

newtype RouteResult = RouteResult
  { routeResult :: Either RouteMismatch Response
  }

type RouteApplicationCont = ContT ResponseReceived IO RouteResult

type RoutingApplication = Request -> RouteApplicationCont

newtype ServantError = ServantError { unServantError :: Text } deriving Show

instance Exception ServantError

type Handler a = ExceptT ServantError IO a

class HasServer layout where
  type Server layout :: Type
  route :: Proxy layout -> Server layout -> RoutingApplication


toApplication :: RoutingApplication -> Application
toApplication raCont request respond = runContT (raCont request) (routerResponse . routeResult)
  where
    routerResponse :: Either RouteMismatch Response -> IO ResponseReceived
    routerResponse (Right response) = respond response

    routerResponse (Left err)       = respond $ responseLBS badRequest400 [] (routeErrorMsg err)

    routeErrorMsg :: RouteMismatch -> LB.ByteString
    routeErrorMsg =  encodeUtf8 @Text @LB.ByteString  . show

serve :: HasServer layout => Proxy layout -> Server layout -> Application
serve proxyApi = toApplication . route proxyApi
