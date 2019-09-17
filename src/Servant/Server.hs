module Servant.Server
  ( module Servant.Server.Internal
  , serve
  ) where

import Network.Wai (Application)

import Servant.Server.Internal

serve :: HasServer layout => Proxy layout -> Server layout -> Application
serve proxyApi server =
  toApplication . runRouter $ route proxyApi $ emptyDelayed (Route server)
