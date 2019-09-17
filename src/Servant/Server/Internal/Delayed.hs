module Servant.Server.Internal.Delayed where

import Network.Wai (Request, Response)
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.Handler
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.ServerError

-- A 'Delayed' contains many delayed blocks of tests, and
-- the actual handler

data Delayed env c where
  Delayed :: { captureD :: env -> DelayedIO captures
             , methodD :: DelayedIO ()
             , acceptD :: DelayedIO ()
             , contentD :: DelayedIO content
             , paramsD :: DelayedIO params
             , headersD :: DelayedIO headers
             , bodyD :: content -> DelayedIO body
             , serverD :: captures
                       -> params
                       -> headers
                       -> body
                       -> Request
                       -> RouteResult c
             } -> Delayed env c

instance Functor (Delayed env) where
  fmap :: (c -> b) -> Delayed env c -> Delayed env b
  fmap f Delayed{..} =
    Delayed
      { serverD = \ c p h b r -> f <$> serverD c p h b r
      , ..
      }

emptyDelayed :: RouteResult a -> Delayed env a
emptyDelayed routeResult =
  Delayed (const r) r r r r r (const r) (\ _ _ _ _ _ ->  routeResult)
  where
    r = return ()

addCapture
  :: Delayed env (a -> b)
  -> (captured -> DelayedIO a) -> Delayed (captured, env) b
addCapture Delayed {..} new =
  Delayed
    { captureD = \ (txt, env) -> (,) <$> captureD env <*> new txt
    , serverD = \ (x, v) p h b r -> ($ v) <$> serverD x p h b r
    , ..
    }

addMethodCheck :: Delayed env a -> DelayedIO () -> Delayed env a
addMethodCheck Delayed{..} check =
  Delayed
   { methodD = methodD <* check
   , ..
   }

addHeadersCheck :: Delayed env (a -> b) -> DelayedIO a -> Delayed env b
addHeadersCheck Delayed {..} new  = Delayed
  { headersD = (,) <$> headersD <*> new
  , serverD = \ c p (h, hNew) b r -> ($ hNew) <$> serverD c p h b r
  , ..
  }

addBodyCheck :: Delayed env (a -> b)
             -> DelayedIO c         -- ^ content type check
             -> (c -> DelayedIO a)  -- ^ body check
             -> Delayed env b
addBodyCheck Delayed{..} newContentD newBodyD =
  Delayed
    { contentD = (,) <$> contentD <*> newContentD
    , bodyD    = \(content, c) -> (,) <$> bodyD content <*> newBodyD c
    , serverD  = \ c p h (z, v) req -> ($ v) <$> serverD c p h z req
    , ..
    } -- Note [Existential Record Update]

addAcceptCheck :: Delayed env a
               -> DelayedIO ()
               -> Delayed env a
addAcceptCheck Delayed{..} new =
  Delayed
    { acceptD = acceptD *> new
    , ..
    } -- Note [Existential Record Update]

-- | Many combinators extract information that is passed to
-- the handler without the possibility of failure. In such a
-- case, 'passToServer' can be used.
passToServer :: Delayed env (a -> b) -> (Request -> a) -> Delayed env b
passToServer Delayed{..} x =
  Delayed
    { serverD = \ c p h b req -> ($ x req) <$> serverD c p h b req
    , ..
    } -- Note [Existential Record Update]

runDelayed :: Delayed env a -> env -> Request -> IO (RouteResult a)
runDelayed Delayed{..} env = runDelayedIO $ do
  r <- ask
  c <- captureD env
  methodD
  acceptD
  content <- contentD
  p <- paramsD       -- Has to be before body parsing, but after content-type checks
  h <- headersD
  b <- bodyD content
  liftRouteResult (serverD c p h b r)

-- | Runs a delayed server and the resulting action.
-- Takes a continuation that lets us send a response.
-- Also takes a continuation for how to turn the
-- result of the delayed server into a response.
runAction :: forall a env r. Delayed env (Handler a)
          -> env
          -> Request
          -> (RouteResult Response -> IO r)
          -> (a -> RouteResult Response)
          -> IO r
runAction action env req respond k =
    runDelayed action env req >>= go >>= respond
  where
    go :: RouteResult (Handler a) -> IO (RouteResult Response)
    go (Fail e)      = return $ Fail e
    go (FailFatal e) = return $ FailFatal e
    go (Route a)     = do
      e <- runHandler a
      case e of
        Left err -> return . Route $ responseServerError err
        Right x  -> return $! k x
