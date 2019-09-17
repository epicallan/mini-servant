{-# LANGUAGE DeriveFunctor #-}
module Servant.Server.Internal.Router where

import Prelude hiding (first)

import Data.Map as Map
import Network.Wai (Response, pathInfo)
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.RoutingApplication
import Servant.Server.Internal.ServerError

-- Router type synonym
type Router env = Router' env RoutingApplication

data Router' env a =
    StaticRouter (Map Text (Router' env a)) [env -> a]
  | CaptureRouter (Router' (Text, env) a)
  | RawRouter (env -> a)
  | Choice (Router' env a) (Router' env a)
  deriving Functor

-- | smart constructor for a single static path component
pathRouter :: Text -> Router env -> Router env
pathRouter path router = StaticRouter (Map.singleton path router) []

-- | smart constructor for a leaf i.e, a router that expects
-- the empty path
leafRouter :: (env -> a) -> Router' env a
leafRouter f = StaticRouter Map.empty [f]

-- | smart constructor for the choice between routers
choice :: Router' env a -> Router' env a -> Router' env a
choice (StaticRouter m1 l1) (StaticRouter m2 l2 ) =
  StaticRouter (Map.unionWith choice m1 m2) (l1 <> l2)
choice (CaptureRouter router1) (CaptureRouter router2) =
  CaptureRouter (choice router1 router2)
choice router1 (Choice router2 router3) = Choice (choice router1 router2) router3
choice router1 router2 = Choice router1 router2

runRouter :: Router () -> RoutingApplication
runRouter r = runRouterEnv r ()

runRouterEnv :: Router env -> env -> RoutingApplication
runRouterEnv router env request respond = case router of
  StaticRouter tables ls ->
    case pathInfo request of
      [] -> runChoice ls env request respond
      -- this is for handling trailing slashes
      [""] -> runChoice ls env request respond
      first : rest | Just router' <- Map.lookup first tables
         -> let request' = request { pathInfo = rest }
            in runRouterEnv router' env request' respond
      _ -> respond $ Fail err404
  CaptureRouter router' ->
    case pathInfo request of
      [] -> respond $ Fail err404
      [""] -> respond $ Fail err404
      first : rest ->
        let request' = request { pathInfo = rest }
        in runRouterEnv router' (first, env) request' respond
  RawRouter app ->
    app env request respond
  Choice r1 r2 -> runChoice [runRouterEnv r1, runRouterEnv r2] env request respond

runChoice :: [env -> RoutingApplication] -> env -> RoutingApplication
runChoice = \case
  [] -> \ _env _req respond -> respond (Fail err404)
  [r] -> r
  (r : rs) ->
    \ env request respond ->
    r env request $ \ response1 ->
      case response1 of
        Fail _ -> runChoice rs env request $ \ response2 ->
                     respond $ higestPriority response1 response2
        _     -> respond response1
  where
   higestPriority
     :: RouteResult Response
     -> RouteResult Response -> RouteResult Response
   higestPriority (Fail e1) (Fail e2) =
     if worseHttpCode (errHTTPCode e1) (errHTTPCode e2) then Fail e2 else Fail e1
   higestPriority (Fail e1) _ = Fail e1
   higestPriority x _ = x

   worseHttpCode :: Int -> Int -> Bool
   worseHttpCode = on (<) toPriority

   toPriority :: Int -> Int
   toPriority 404 = 0 -- not found
   toPriority 405 = 1 -- method not allowed
   toPriority 401 = 2 -- unauthorized
   toPriority 415 = 3 -- unsupported media type
   toPriority 406 = 4 -- not acceptable
   toPriority 400 = 6 -- bad request
   toPriority _   = 5
