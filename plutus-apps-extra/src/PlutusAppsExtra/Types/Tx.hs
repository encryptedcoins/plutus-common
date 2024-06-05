{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.Types.Tx where

import           Cardano.Api                      (FromJSON, ToJSON)
import           Control.Monad.State              (State, execState, gets, modify)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           GHC.Generics                     (Generic)
import           Ledger.Typed.Scripts             (Any, ValidatorTypes (..))
import           Plutus.V2.Ledger.Api             (POSIXTime)
import           PlutusTx.Prelude                 hiding (Semigroup, fromInteger, mapMaybe, mempty, toList, unless, (<$>))
import           Prelude                          (Monoid (mempty), Show)
import qualified Prelude                          as Haskell

import           PlutusAppsExtra.PlutusApps       (ScriptLookups, TxConstraints)
import           PlutusAppsExtra.IO.Time          (HasClock (..))
import           PlutusAppsExtra.Types.Error      (TxBuilderError)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)

data TxConstructor a i o = TxConstructor
    {
        txCurrentTime          :: POSIXTime,
        txConstructorLookups   :: MapUTXO,
        txConstructorErrors    :: [TxBuilderError],
        txConstructorResult    :: Maybe (ScriptLookups a, TxConstraints i o),
        txUtxoRequirements     :: UtxoRequirements
    }
    deriving (Show, Generic, FromJSON, ToJSON)

type Transaction = TxConstructor Any (RedeemerType Any) (DatumType Any)
type TransactionBuilder a = State Transaction a

mkTxConstructor :: POSIXTime -> MapUTXO -> Transaction
mkTxConstructor ct lookups = TxConstructor ct lookups [] (Just (mempty, mempty)) mempty

selectTxConstructor :: [Transaction] -> Maybe Transaction
selectTxConstructor = find (isJust . txConstructorResult)

buildTxConstraints :: TransactionBuilder () -> Transaction ->
    Maybe (ScriptLookups Any, TxConstraints (RedeemerType Any) (DatumType Any))
buildTxConstraints builder tx = txConstructorResult $ builder `execState` tx

------------------------------------------------ Helper functions ------------------------------------------------

getBuilderTime :: TransactionBuilder POSIXTime
getBuilderTime = gets txCurrentTime

getBuilderUtxos :: TransactionBuilder MapUTXO
getBuilderUtxos = gets txConstructorLookups

getBuilderErrors :: TransactionBuilder [TxBuilderError]
getBuilderErrors = gets txConstructorErrors

getBuilderResult :: TransactionBuilder (Maybe (ScriptLookups Any, TxConstraints (RedeemerType Any) (DatumType Any)))
getBuilderResult = gets txConstructorResult

------------------------------------------------ Utxo requirements ------------------------------------------------

txBuilderRequirements :: [TransactionBuilder ()] -> Haskell.IO UtxoRequirements
txBuilderRequirements txs = do
    ct <- currentTime
    let constrInit = mkTxConstructor ct mempty
        constrs = map (`execState` constrInit) txs
    Haskell.pure $ Haskell.mconcat $ fmap txUtxoRequirements constrs

data UtxoRequirement
    = RequiresDatum
    | RequiresScript
    | RequiresValidator
    deriving (Show, Generic, FromJSON, ToJSON, Haskell.Eq, Haskell.Ord)

type UtxoRequirements = Set UtxoRequirement

allRequirements :: UtxoRequirements
allRequirements = Set.fromList [RequiresDatum, RequiresScript, RequiresValidator]

requiresDatum, requiresScript, requiresValidator :: TransactionBuilder ()
requiresDatum     = requires RequiresDatum
requiresScript    = requires RequiresScript
requiresValidator = requires RequiresValidator

requires :: UtxoRequirement -> TransactionBuilder ()
requires req = modify $ \TxConstructor{..} -> TxConstructor{txUtxoRequirements = Set.insert req txUtxoRequirements, ..}