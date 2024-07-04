{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry"            #-}
{-# LANGUAGE FlexibleInstances     #-}

-- | Implements a custom currency with a minting policy that allows
--   the minting of a fixed amount of units.
module PlutusAppsExtra.Scripts.OneShotCurrency where

import           Data.Aeson                           (FromJSON, ToJSON)
import           GHC.Generics                         (Generic)

import           Plutus.Script.Utils.Typed            (mkUntypedMintingPolicy)
import           Plutus.Script.Utils.V2.Scripts       (MintingPolicy, scriptCurrencySymbol)
import           PlutusLedgerApi.V2                   (CurrencySymbol, ScriptContext (..), TokenName, TxInfo (..), TxOutRef (..), singleton)
import qualified PlutusTx
import qualified PlutusTx.AssocMap                    as AssocMap
import           PlutusTx.Prelude                     hiding (Monoid (..), Semigroup (..))
import qualified Prelude                              as Haskell

import           Ledger                               (DecoratedTxOut, Language (..), Versioned (Versioned), mkMintingPolicyScript)
import           Plutus.Script.Utils.V2.Contexts      (spendsOutput)
import           PlutusAppsExtra.Constraints.OffChain (tokensMintedTx, utxoSpentPublicKeyTx)
import           PlutusAppsExtra.Types.Tx             (TransactionBuilder)
import           PlutusCore.Builtin.Debug             (plcVersion100)
import           PlutusLedgerApi.V1                   (TokenName (..))
import qualified PlutusLedgerApi.V2                   as P
import           PlutusLedgerApi.V2.Contexts          (ownCurrencySymbol)
import qualified PlutusTx.AssocMap                    as PMAP
import           UntypedPlutusCore                    (Version)

---------------------------------- Types ------------------------------------

data OneShotCurrencyParams = OneShotCurrencyParams
    {
        curRef     :: TxOutRef,
        curAmounts :: AssocMap.Map TokenName Integer
    }
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''OneShotCurrencyParams

-------------------------------- On-Chain ------------------------------------

oneShotCurrencyValue :: CurrencySymbol -> OneShotCurrencyParams -> P.Value
oneShotCurrencyValue s OneShotCurrencyParams{curAmounts = amts} =
    let values = map (\(tn, i) -> singleton s tn i) (AssocMap.toList amts)
    in fold values

checkPolicy :: OneShotCurrencyParams -> () -> ScriptContext -> Bool
checkPolicy c@(OneShotCurrencyParams (TxOutRef refHash refIdx) _) _ ctx@ScriptContext{scriptContextTxInfo=txinfo} =
    let ownSymbol = ownCurrencySymbol ctx

        minted = txInfoMint txinfo
        expected = oneShotCurrencyValue ownSymbol c

        -- True if the pending transaction mints the amount of
        -- currency that we expect
        mintOK =
            let v = expected == minted
            in traceIfFalse "C0" {-"Value minted different from expected"-} v

        -- True if the pending transaction spends the output
        -- identified by @(refHash, refIdx)@
        txOutputSpent =
            let v = spendsOutput txinfo refHash refIdx
            in  traceIfFalse "C1" {-"Pending transaction does not spend the designated transaction output"-} v

    in mintOK && txOutputSpent

oneShotCurrencyPolicy :: Version -> OneShotCurrencyParams ->  MintingPolicy
oneShotCurrencyPolicy ver cur = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy . checkPolicy ||])
        `PlutusTx.unsafeApplyCode`
            PlutusTx.liftCode ver cur

-------------------------------- Off-Chain -----------------------------------

mkCurrency :: TxOutRef -> [(TokenName, Integer)] -> OneShotCurrencyParams
mkCurrency ref amts =
    OneShotCurrencyParams
        {
            curRef     = ref,
            curAmounts = PMAP.unsafeFromList amts
        }

currencySymbol :: OneShotCurrencyParams -> CurrencySymbol
currencySymbol = scriptCurrencySymbol . oneShotCurrencyPolicy plcVersion100

currencyValue :: OneShotCurrencyParams -> P.Value
currencyValue cur = (`oneShotCurrencyValue` cur) $ currencySymbol cur

-- Constraints that the OneShotCurrency is minted in the transaction
oneShotCurrencyMintTx :: OneShotCurrencyParams -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
oneShotCurrencyMintTx par@(OneShotCurrencyParams ref _) = do
    tokensMintedTx (Versioned (oneShotCurrencyPolicy plcVersion100 par) PlutusV2) () (currencyValue par)
    utxoSpentPublicKeyTx (\r _ -> r == ref)

----------------------------------------------------------------------------------------------

checkPolicy' :: () -> ScriptContext -> Bool
checkPolicy' _ ctx@ScriptContext{scriptContextTxInfo=txinfo} =
    let ownSymbol = ownCurrencySymbol ctx

        minted = txInfoMint txinfo
        expected = fold (map (\(tn, i) -> singleton ownSymbol tn i) ([(TokenName "", 1)]))

        -- True if the pending transaction mints the amount of
        -- currency that we expect
        mintOK =
            let v = expected == minted
            in traceIfFalse "C0" {-"Value minted different from expected"-} v

        -- -- True if the pending transaction spends the output
        -- -- identified by @(refHash, refIdx)@
        txOutputSpent =
            let v = spendsOutput txinfo (P.TxId "f25655007d89a1459fc0b2933e89888a7955dc8560e0440cb371a5c0a45d5967") 2
            in  traceIfFalse "C1" {-"Pending transaction does not spend the designated transaction output"-} v

    in mintOK && txOutputSpent
