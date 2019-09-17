module Servant.API.Sub ( (:>))  where

infixr 4 :>
data (path :: k) :> (sublayout :: Type)
