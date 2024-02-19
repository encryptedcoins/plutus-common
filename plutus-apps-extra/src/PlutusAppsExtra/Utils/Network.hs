module PlutusAppsExtra.Utils.Network where

import Cardano.Api (NetworkId)

class Monad m => HasNetworkId m where
    getNetworkId :: m NetworkId