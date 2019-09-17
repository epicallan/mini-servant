{-# LANGUAGE DeriveFunctor #-}
module Servant.Server.Internal.DelayedIO where

import Network.Wai (Request)
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.ServerError

newtype DelayedIO a = DelayedIO { runDelayedIO' :: ReaderT Request (RouteResultT IO) a}
  deriving ( Functor, Applicative, Monad, MonadThrow
           , MonadIO, MonadReader Request
           )

liftRouteResult :: RouteResult a -> DelayedIO a
liftRouteResult = DelayedIO . lift .  RouteResultT . return

runDelayedIO :: DelayedIO a -> Request -> IO (RouteResult a)
runDelayedIO m = runRouteResult . runReaderT (runDelayedIO' m)

delayedFail :: ServerError -> DelayedIO a
delayedFail = liftRouteResult . Fail

delayedFailFatal :: ServerError -> DelayedIO a
delayedFailFatal = liftRouteResult . FailFatal

withRequest :: (Request -> DelayedIO a) -> DelayedIO a
withRequest = (ask >>=)
