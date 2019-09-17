module Servant.API.Capture where

import GHC.TypeLits (Symbol)


data Capture (symb :: Symbol) (captureType :: Type)

data CaptureAll (sym :: Symbol) (a :: *)
-- ^ TODO: cross check for CaptureAll interpretation instance
    deriving (Typeable)
