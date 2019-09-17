
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}

module Servant.API.Alternative ((:<|>)(..)) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Semigroup (Semigroup (..))
import Data.Typeable (Typeable)


-- | Union of two APIs, first takes precedence in case of overlap.
--
-- Example:
--
-- >>> :{
--type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
--        :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] () -- POST /books
-- :}
data a :<|> b = a :<|> b
    deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 3 :<|>

instance (Semigroup a, Semigroup b) => Semigroup (a :<|> b) where
    (a :<|> b) <> (a' :<|> b') = (a <> a') :<|> (b <> b')

instance (Monoid a, Monoid b) => Monoid (a :<|> b) where
    mempty = mempty :<|> mempty
    mappend = (<>)

instance Bifoldable (:<|>) where
-- ^ TODO: read about Bifoldable class
    bifoldMap f g ~(a :<|> b) = f a `mappend` g b

instance Bifunctor (:<|>) where
    bimap f g ~(a :<|> b) = f a :<|> g b

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
