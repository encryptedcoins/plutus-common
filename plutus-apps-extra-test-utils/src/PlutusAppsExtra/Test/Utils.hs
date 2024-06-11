{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module PlutusAppsExtra.Test.Utils
    ( module PlutusAppsExtra.Test.Utils.Gen
    , module PlutusAppsExtra.Test.Utils.Script
    , module PlutusAppsExtra.Test.Utils.Tx
    , getProtocolParams
    , isOutOfResoursesError
    ) where

import           Cardano.Api                                (NetworkId)
import qualified Cardano.Api.Shelley                        as C
import           Cardano.Node.Emulator.Internal.Node.Params (Params (..), emulatorAlonzoGenesisDefaults, emulatorConwayGenesisDefaults,
                                                             emulatorEpochSize, emulatorShelleyGenesisDefaults, pParamsFromProtocolParams)
import qualified Cardano.Node.Emulator.Internal.Node.Params as C
import           Data.Aeson                                 (eitherDecodeFileStrict)
import           Data.Default                               (def)
import           Data.List                                  (isInfixOf)
import           Ledger                                     (CardanoTx, ScriptError (EvaluationError), ValidationError (ScriptFailure),
                                                             ValidationPhase (Phase2))
import           PlutusAppsExtra.Test.Utils.Gen
import           PlutusAppsExtra.Test.Utils.Script
import           PlutusAppsExtra.Test.Utils.Tx
import           PlutusAppsExtra.Types.Error                (BalanceExternalTxError (MakeAutoBalancedTxError))

getProtocolParams :: FilePath -> NetworkId -> IO Params
getProtocolParams fp networkId = do
    pp <- either error id <$> eitherDecodeFileStrict @C.ProtocolParameters fp
    let tconf = C.mkLatestTransitionConfig
            emulatorShelleyGenesisDefaults
            emulatorAlonzoGenesisDefaults
            emulatorConwayGenesisDefaults
    pure $ Params
        { pSlotConfig      = def
        , pEmulatorPParams = pParamsFromProtocolParams pp
        , pNetworkId       = networkId
        , pEpochSize       = emulatorEpochSize
        , pConfig          = tconf
        }

isOutOfResoursesError :: Either BalanceExternalTxError CardanoTx -> Bool
isOutOfResoursesError = \case
    Left (MakeAutoBalancedTxError (Left (Phase2, ScriptFailure (EvaluationError _ txt)))) -> msg `isInfixOf` txt
    _ -> False
    where
        msg = "The machine terminated part way through evaluation due to overspending the budget."
