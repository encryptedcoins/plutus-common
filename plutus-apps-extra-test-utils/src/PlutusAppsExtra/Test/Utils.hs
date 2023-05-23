module PlutusAppsExtra.Test.Utils
    ( module PlutusAppsExtra.Test.Utils.Gen
    , module PlutusAppsExtra.Test.Utils.Script
    , module PlutusAppsExtra.Test.Utils.Tx
    , getProtocolParams
    ) where

import           Cardano.Api                    (NetworkId)
import           Cardano.Node.Emulator          (Params (..), pParamsFromProtocolParams)
import           Data.Aeson                     (eitherDecodeFileStrict)
import           Data.Default                   (def)
import           PlutusAppsExtra.Test.Utils.Gen
import           PlutusAppsExtra.Test.Utils.Script
import           PlutusAppsExtra.Test.Utils.Tx

getProtocolParams :: FilePath -> NetworkId -> IO Params
getProtocolParams fp networkId = do
    pp <- either error id <$> eitherDecodeFileStrict fp
    pure $ Params def (pParamsFromProtocolParams pp) networkId