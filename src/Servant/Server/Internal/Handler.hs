module Servant.Server.Internal.Handler where

import Control.Monad.Error.Class (MonadError)
import Servant.Server.Internal.ServerError

newtype Handler a = Handler {runHandler' :: ExceptT ServerError IO a}
  deriving ( Functor, Applicative, Monad
           , MonadError ServerError, MonadThrow
           , MonadCatch, MonadMask
           )

runHandler :: Handler a -> IO (Either ServerError a)
runHandler = runExceptT . runHandler'
