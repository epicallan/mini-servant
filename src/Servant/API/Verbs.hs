{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TypeApplications #-}
module Servant.API.Verbs
         ( Verb
         , Get
         , Post
         ) where
import Data.Aeson (ToJSON, encode)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.TypeLits (Nat)
import GHC.TypeNats (KnownNat)
import Network.HTTP.Types (Status)
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Method (Method, StdMethod (..), methodGet, methodPost)
import Network.Wai (pathInfo, requestMethod, responseLBS)
import Servant.Server (Handler, HasServer (..), RouteMismatch (..), RouteResult (..),
                       RoutingApplication)

data Verb (method :: k) (status :: Nat) (contentType :: Type)

type Get c = Verb 'GET 200 c

type Post c = Verb 'POST 200 c

class ReflectMethod (a :: StdMethod) where
  reflectMethod :: Proxy a -> Method

instance ReflectMethod 'GET where
  reflectMethod _ = methodGet

instance ReflectMethod 'POST where
  reflectMethod _ = methodPost

instance (ToJSON contentType, KnownNat status, ReflectMethod method)
  => HasServer (Verb method status contentType) where

  type Server (Verb method status contentType) = Handler contentType

  route :: Proxy (Verb method status contentType) -> Handler contentType -> RoutingApplication
  route _ action request
    | null (pathInfo request) && requestMethod request == reflectMethod (Proxy @method) = do
      eResult <- liftIO $ runExceptT action
      return $ RouteResult $ case eResult of
        Left _       ->  Left NotFound -- TODO: use serverError
        Right result -> Right . responseLBS responseStatus jsonHeader $ encode result
    | otherwise = return . RouteResult $ Left NotFound

    where
      jsonHeader :: ResponseHeaders
      jsonHeader =  [("Content-Type", "application/json")]

      statusCode :: Int
      statusCode = fromIntegral $ natVal (Proxy @status)

      responseStatus :: Status
      responseStatus = toEnum statusCode
