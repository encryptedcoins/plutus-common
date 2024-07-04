{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Tests.Scripts where

import           Cardano.Api                          (NetworkId (..), NetworkMagic (..))
import           Cardano.Ledger.Alonzo                (Alonzo)
import qualified Cardano.Ledger.Plutus.Language       as Plutus
import           Cardano.Node.Emulator                (Params (..))
import           Data.Aeson                           (decode)
import           Data.ByteString.Lazy                 (readFile)
import           Data.Default                         (def)
import           Data.Maybe                           (fromJust)
import           Data.Text                            (Text)
import           Ledger                               (Script (unScript), ScriptContext (..), ScriptPurpose (..), TxInfo (..),
                                                       TxOutRef (..), always, unValidatorScript)
import           Ledger.Typed.Scripts                 (IsScriptContext (..), Validator)
import           Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes (..), mkTypedValidator)
import           PlutusAppsExtra.Constraints.OnChain  (tokensMinted)
import           PlutusAppsExtra.Test.Utils           (emptyTxInfoPV1, emptyTxInfoPV2, getProtocolParams, testValidator)
import           PlutusAppsExtra.Utils.Scripts        (unsafeParameterizedValidatorFromCBOR, unsafeValidatorFromCBOR)
import           PlutusCore.Core                      (plcVersion100)
import           PlutusLedgerApi.Common               (PlutusLedgerLanguage (PlutusV2), serialisedScript, uncheckedDeserialiseUPLC)
import           PlutusLedgerApi.V1                   (TxId (..))
import qualified PlutusLedgerApi.V1.Contexts          as PV1
import qualified PlutusLedgerApi.V2.Contexts          as PV2
import           PlutusTx                             (compile)
import           PlutusTx.AssocMap                    (empty)
import           PlutusTx.Prelude                     (emptyByteString, zero)
import           Prelude                              hiding (readFile)

-- Redeemer is equal to (42 :: Integer)
validatorUPLC1 :: Text
validatorUPLC1 = "5819010000322223253330053370e002902a0a4c2c6eb40095cd01"

validator1 :: Validator
validator1 = unsafeValidatorFromCBOR @Plutus.PlutusV1 @Alonzo validatorUPLC1

-- Redeemer is equal to the script parameter (Integer)
validatorUPLC2 :: Text
validatorUPLC2 = "581d010000322322223253330073370e00200a2930b1bad002375a002ae681"

validator2 :: Integer -> Validator
validator2 = unsafeParameterizedValidatorFromCBOR @Plutus.PlutusV1 @Alonzo validatorUPLC2

------------------------------------------------------------------------------------------------------

runScriptTest :: FilePath -> IO ()
runScriptTest path = do
  let networkId = Testnet $ NetworkMagic 1
      ctx = PV1.ScriptContext emptyTxInfoPV1 (Spending $ TxOutRef "" 0)
  params <- getProtocolParams path networkId

  testValidator @Plutus.PlutusV1 @Alonzo validator1 () (42 :: Integer) ctx

  testValidator @Plutus.PlutusV1 @Alonzo (validator2 (42 :: Integer)) () (42 :: Integer) ctx

  print "Success!"
