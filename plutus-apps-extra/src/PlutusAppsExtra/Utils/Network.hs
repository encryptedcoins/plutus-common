module PlutusAppsExtra.Utils.Network where

import           Cardano.Api (NetworkId)
import qualified Cardano.Api as Api

class Monad m => HasNetworkId m where
    getNetworkId :: m NetworkId

networkIdToEpochSlots :: NetworkId -> Maybe Api.EpochSlots
networkIdToEpochSlots (Api.Testnet (Api.NetworkMagic 42))         = Just $ Api.EpochSlots 500
networkIdToEpochSlots  Api.Mainnet                                = Just $ Api.EpochSlots 432000
networkIdToEpochSlots (Api.Testnet (Api.NetworkMagic 1))          = Just $ Api.EpochSlots 432000
networkIdToEpochSlots (Api.Testnet (Api.NetworkMagic 2))          = Just $ Api.EpochSlots 86400
networkIdToEpochSlots (Api.Testnet (Api.NetworkMagic 1097911063)) = Just $ Api.EpochSlots 432000
networkIdToEpochSlots _                                           = Nothing
