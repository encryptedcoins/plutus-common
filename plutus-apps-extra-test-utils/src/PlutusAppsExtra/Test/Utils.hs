{-# LANGUAGE LambdaCase #-}

module PlutusAppsExtra.Test.Utils
    ( module PlutusAppsExtra.Test.Utils.Gen
    , module PlutusAppsExtra.Test.Utils.Script
    , module PlutusAppsExtra.Test.Utils.Tx
    , getProtocolParams
    , isOutOfResoursesError
    ) where

import           Cardano.Api                       (NetworkId)
import           Cardano.Node.Emulator             (Params (..), pParamsFromProtocolParams)
import           Data.Aeson                        (eitherDecodeFileStrict)
import           Data.Default                      (def)
import           Data.List                         (isInfixOf)
import           Ledger                            (CardanoTx, ScriptError (EvaluationError), ValidationError (ScriptFailure),
                                                    ValidationPhase (Phase2))
import           PlutusAppsExtra.Test.Utils.Gen
import           PlutusAppsExtra.Test.Utils.Script
import           PlutusAppsExtra.Test.Utils.Tx
import           PlutusAppsExtra.Types.Error       (BalanceExternalTxError (MakeAutoBalancedTxError))

getProtocolParams :: FilePath -> NetworkId -> IO Params
getProtocolParams fp networkId = do
    pp <- either error id <$> eitherDecodeFileStrict fp
    pure $ Params def (pParamsFromProtocolParams pp) networkId

isOutOfResoursesError :: Either BalanceExternalTxError CardanoTx -> Bool
isOutOfResoursesError = \case
    Left (MakeAutoBalancedTxError (Left (Phase2, ScriptFailure (EvaluationError _ txt)))) -> msg `isInfixOf` txt
    _ -> False
    where
        msg = "The machine terminated part way through evaluation due to overspending the budget."