{-# LANGUAGE DeriveFunctor #-}
module Servant.Server.Internal.RouteResult where

import qualified Control.Monad.Catch as CM (MonadThrow (..))
import Servant.Server.Internal.ServerError

-- | The result of matching against a path in the route tree.
data RouteResult a =
    Fail ServerError           -- ^ Keep trying other paths.
                               --   The 'ServantError' should only be 404, 405 or 406.
  | FailFatal !ServerError     -- ^ Don't try other paths.
  | Route !a
  deriving (Eq, Show, Read, Functor)

instance Applicative RouteResult where
    pure = return
    (<*>) = ap

instance Monad RouteResult where
  return = Route

  (>>=) :: RouteResult a -> (a -> RouteResult b) -> RouteResult b
  Route a >>= f = f a
  Fail e >>= _ = Fail e
  FailFatal e >>= _ = FailFatal e

newtype RouteResultT m a = RouteResultT { runRouteResult :: m (RouteResult a)}
  deriving (Functor)

instance MonadTrans RouteResultT where
  lift :: Monad m => m a -> RouteResultT m a
  lift = RouteResultT . fmap Route

instance (Applicative f, Monad f) => Applicative (RouteResultT f) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (RouteResultT m) where
  return = RouteResultT . return . Route

  (>>=) :: RouteResultT m a -> (a -> RouteResultT m b) -> RouteResultT m b
  ma >>= f = RouteResultT $ do
    a <- runRouteResult ma
    case a of
      Route b     -> runRouteResult $ f b
      Fail e      -> return $ Fail e
      FailFatal e -> return $ FailFatal e

instance MonadIO m => MonadIO (RouteResultT m) where
  liftIO :: IO a -> RouteResultT m a
  liftIO = lift . liftIO

instance CM.MonadThrow m => CM.MonadThrow (RouteResultT m) where
    throwM = lift . throwM
