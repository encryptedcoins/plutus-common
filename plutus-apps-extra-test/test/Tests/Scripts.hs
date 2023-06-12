{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Tests.Scripts where

import           Cardano.Node.Emulator                (Params(..), pParamsFromProtocolParams)
import           Cardano.Api                          (NetworkId(..), NetworkMagic (..))
import           Data.Aeson                           (decode)
import           Data.ByteString.Lazy                 (readFile)
import           Data.Default                         (def)
import           Data.Maybe                           (fromJust)
import           Data.Text                            (Text)
import           Ledger.Typed.Scripts                 (IsScriptContext (..), Validator)
import           Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes (..), mkTypedValidator)
import           Plutus.V2.Ledger.Api
import           PlutusTx                             (compile)
import           PlutusTx.AssocMap                    (fromList, empty)
import           PlutusTx.Prelude                     (zero, emptyByteString)
import           Prelude                              hiding (readFile)

import           PlutusAppsExtra.Constraints.OnChain  (tokensMinted)
import           PlutusAppsExtra.Test.Utils           (testValidator)
import           PlutusAppsExtra.Utils.Scripts        (unsafeValidatorFromCBOR, unsafeParameterizedValidatorFromCBOR)

-- Redeemer is equal to (42 :: Integer)
validatorUPLC1 :: Text
validatorUPLC1 = "5819010000322223253330053370e002902a0a4c2c6eb40095cd01"

validator1 :: Validator
validator1 = unsafeValidatorFromCBOR validatorUPLC1

-- Redeemer is equal to the script parameter (Integer)
validatorUPLC2 :: Text
validatorUPLC2 = "581d010000322322223253330073370e00200a2930b1bad002375a002ae681"

validator2 :: Integer -> Validator
validator2 = unsafeParameterizedValidatorFromCBOR validatorUPLC2

------------------------------------------------------------------------------------------------------

runScriptTest :: FilePath -> IO ()
runScriptTest path = do
  pp <- fromJust . decode <$> readFile path
  let networkId = Testnet $ NetworkMagic 1
      ledgerParams = Params def (pParamsFromProtocolParams pp) networkId

  let info = TxInfo {
    txInfoInputs = [],
    txInfoReferenceInputs = [],
    txInfoOutputs = [],
    txInfoFee = zero,
    txInfoMint = zero,
    txInfoDCert = [],
    txInfoWdrl = empty,
    txInfoValidRange = always,
    txInfoSignatories = [],
    txInfoRedeemers = empty,
    txInfoData = empty,
    txInfoId = TxId emptyByteString
  }

  let ctx = ScriptContext info (Spending $ TxOutRef (TxId emptyByteString) 0)

  testValidator ledgerParams validator1 () (42 :: Integer) ctx

  testValidator ledgerParams (validator2 (42 :: Integer)) () (42 :: Integer) ctx
  
  print "Success!"