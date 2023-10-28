{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.Constraints.OffChain where

import           Control.Monad                          (liftM2, when)
import           Control.Monad.State                    (MonadState (..))
import           Data.Functor                           (($>))
import           Data.List                              (find)
import qualified Data.Map                               as Map
import           Data.Maybe                             (isJust, isNothing)
import           Data.Text                              (Text)
import           Ledger                                 (DecoratedTxOut (..), Slot, Versioned, mintingPolicyHash, validatorHash)
import           Ledger.Address                         (PaymentPubKeyHash)
import           Plutus.V2.Ledger.Api                   (Address (Address), Credential (PubKeyCredential, ScriptCredential),
                                                         Datum (Datum), MintingPolicy (..), POSIXTime, PubKeyHash,
                                                         Redeemer (Redeemer), StakingCredential, ToData (..), TxOutRef,
                                                         Validator (..), ValidatorHash)
import qualified Plutus.V2.Ledger.Api                   as P                                                     
import           Prelude

import           Ledger.Tx.Constraints                  (TxConstraint (MustPayToAddress), mintingPolicy, mustBeSignedBy,
                                                         mustPayToAddressWithReferenceMintingPolicy,
                                                         mustPayToAddressWithReferenceValidator, mustReferenceOutput,
                                                         mustSpendPubKeyOutput, mustSpendScriptOutput,
                                                         mustSpendScriptOutputWithReference, mustUseOutputAsCollateral,
                                                         mustValidateInSlotRange, mustValidateInTimeRange, otherData, otherScript,
                                                         unspentOutputs)
import           Ledger.Tx.Constraints.TxConstraints    (TxOutDatum, mustMintValueWithRedeemerAndReference, singleton)
import           Ledger.Tx.Constraints.ValidityInterval (interval)
import           PlutusAppsExtra.Types.Error            (TxBuilderError (..))
import           PlutusAppsExtra.Types.Tx               (TransactionBuilder, TxConstructor (..), getBuilderResult, requiresScript, requiresValidator)
import           PlutusAppsExtra.Utils.ChainIndex       (filterPubKeyUtxos, filterScriptUtxos)

(<&&>) :: (Semigroup a, Monad m) => m a -> m a -> m a
(<&&>) = liftM2 (<>)

-- If Nothing is passed as the 3rd argument, adds the specified error to the list and sets txConstructorResult to Nothing.
failTx :: Text -> Text -> Maybe res -> TransactionBuilder (Maybe res)
failTx eIn eReason r = if isJust r
    then return r
    else do
        constr <- get
        let errorList = txConstructorErrors constr
        put constr { txConstructorErrors = TxBuilderError eIn eReason : errorList, txConstructorResult = Nothing }
        return r

utxosSpentPublicKeyTx :: [TxOutRef] -> TransactionBuilder ()
utxosSpentPublicKeyTx refs = do
    mapM_ (\ref -> utxoSpentPublicKeyTx (\r _ -> ref == r)) refs
    res <- getBuilderResult
    when (isNothing res) $ failTx "utxosSpentPublicKeyTx" "Cannot spend all provided references" Nothing $> ()

utxoSpentPublicKeyTx :: (TxOutRef -> DecoratedTxOut -> Bool) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentPublicKeyTx f = utxoSpentPublicKeyTx' f >>= failTx "utxoSpentPublicKeyTx" "No matching utxos found"

utxoSpentPublicKeyTx' :: (TxOutRef -> DecoratedTxOut -> Bool) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentPublicKeyTx' f = do
    constr <- get
    let utxos   = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Map.filterWithKey f $ filterPubKeyUtxos utxos
    if Map.null utxos'
        then return Nothing
        else do
            let utxo = head $ Map.toList utxos'
                ref  = fst utxo
                lookups = unspentOutputs (Map.fromList [utxo])
                cons    = mustSpendPubKeyOutput ref
            put constr  {
                            txConstructorResult = res <&&> Just (lookups, cons),
                            txConstructorLookups = Map.delete ref utxos
                        }
            return $ Just utxo

utxoSpentScriptTx :: ToData redeemer => (TxOutRef -> DecoratedTxOut -> Bool) -> (TxOutRef -> DecoratedTxOut -> Versioned Validator) ->
    (TxOutRef -> DecoratedTxOut -> redeemer) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentScriptTx f scriptVal red = utxoSpentScriptTx' f scriptVal red >>= failTx "utxoSpentScriptTx" "No matching utxos found"

utxoSpentScriptTx' :: ToData redeemer => (TxOutRef -> DecoratedTxOut -> Bool) -> (TxOutRef -> DecoratedTxOut -> Versioned Validator) ->
    (TxOutRef -> DecoratedTxOut -> redeemer) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentScriptTx' f scriptVal red = do
    requiresScript
    requiresValidator
    constr <- get
    let utxos   = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Map.filterWithKey f $ filterScriptUtxos utxos
    if Map.null utxos'
        then return Nothing
        else do
            let utxo    = head $ Map.toList utxos'
                ref     = fst utxo
                val     = uncurry scriptVal utxo
                r       = Redeemer $ toBuiltinData $ uncurry red utxo
                script  = fmap getValidator val
                mutxo'  = find (\(_, o) -> _decoratedTxOutReferenceScript o == Just script) $ Map.toList utxos
                lookups = case mutxo' of
                    Nothing    -> unspentOutputs (Map.fromList [utxo]) <> otherScript val
                    Just utxo' -> unspentOutputs (Map.fromList [utxo, utxo']) <> otherScript val
                cons    = case mutxo' of
                    Nothing    -> mustSpendScriptOutput ref r
                    Just utxo' -> mustSpendScriptOutputWithReference ref r (fst utxo')
            put constr  {
                            txConstructorResult = res <&&> Just (lookups, cons),
                            txConstructorLookups = Map.delete ref utxos
                        }
            return $ Just utxo

-- This function is for datum referencing
utxoReferencedTx :: (TxOutRef -> DecoratedTxOut -> Bool) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoReferencedTx f = utxoReferencedTx' f >>= failTx "utxoReferencedTx" "No matching utxos found"

-- This function is for datum referencing
utxoReferencedTx' :: (TxOutRef -> DecoratedTxOut -> Bool) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoReferencedTx' f = do
    constr <- get
    let utxos   = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Map.filterWithKey f utxos
    if Map.null utxos'
        then return Nothing
        else do
            let utxo    = head $ Map.toList utxos'
                ref     = fst utxo
                lookups = unspentOutputs (Map.fromList [utxo])
                cons    = mustReferenceOutput ref
            put constr { txConstructorResult = res <&&> Just (lookups, cons) }
            return $ Just utxo

useAsCollateralTx :: Maybe TxOutRef -> TransactionBuilder (Maybe TxOutRef)
useAsCollateralTx ref = useAsCollateralTx' ref >>= failTx "useAsCollateralTx" "No matching utxos found"

-- TODO: Check if the utxo is locked by a public key
useAsCollateralTx' :: Maybe TxOutRef -> TransactionBuilder (Maybe TxOutRef)
useAsCollateralTx' Nothing    = return Nothing
useAsCollateralTx' (Just ref) = do
    constr <- get
    let utxos = txConstructorLookups constr
        res   = txConstructorResult constr
    case Map.lookup ref utxos of
        Nothing -> return Nothing
        Just o  -> do
            let lookups = unspentOutputs (Map.fromList [(ref, o)])
                cons    = mustUseOutputAsCollateral ref
            put constr  {
                            txConstructorResult = res <&&> Just (lookups, cons),
                            txConstructorLookups = Map.delete ref utxos
                        }
            return $ Just ref

utxoProducedTx :: Address -> P.Value -> Maybe (TxOutDatum Datum) -> TransactionBuilder ()
utxoProducedTx addr val dat = do
    constr <- get
    let res = txConstructorResult constr
        c    = singleton (MustPayToAddress addr dat Nothing val)
    put constr { txConstructorResult = res <&&> Just (mempty, c) }

utxoProducedPublicKeyTx :: PubKeyHash -> Maybe StakingCredential -> P.Value -> Maybe (TxOutDatum Datum) -> TransactionBuilder ()
utxoProducedPublicKeyTx pkh skc val dat =
    let addr = Address (PubKeyCredential pkh) skc
    in utxoProducedTx addr val dat

utxoProducedScriptTx :: ValidatorHash -> Maybe StakingCredential -> P.Value -> TxOutDatum Datum -> TransactionBuilder ()
utxoProducedScriptTx vh skc val dat =
    let addr = Address (ScriptCredential vh) skc
    in utxoProducedTx addr val (Just dat)

tokensMintedTx :: ToData redeemer => Versioned MintingPolicy -> redeemer -> P.Value -> TransactionBuilder ()
tokensMintedTx mp red v = do
    requiresScript
    constr <- get
    let res     = txConstructorResult constr
        -- Attempting to find a reference script
        utxos   = txConstructorLookups constr
        script  = fmap getMintingPolicy mp
        mutxo   = find (\(_, o) -> _decoratedTxOutReferenceScript o == Just script) $ Map.toList utxos
        lookups = case mutxo of
            Nothing   -> mintingPolicy mp
            Just utxo -> mintingPolicy mp <> unspentOutputs (Map.fromList [utxo])
        cons    = mustMintValueWithRedeemerAndReference (Redeemer $ toBuiltinData red) (fst <$> mutxo) v
    put constr { txConstructorResult = res <&&> Just (lookups,  cons)}

validatedInTimeIntervalTx :: POSIXTime -> POSIXTime -> TransactionBuilder ()
validatedInTimeIntervalTx startTime endTime = do
    constr <- get
    let res  = txConstructorResult constr
    put constr { txConstructorResult = res <&&> Just (mempty, mustValidateInTimeRange $ interval startTime endTime) }

validatedInSlotIntervalTx :: Slot -> Slot -> TransactionBuilder ()
validatedInSlotIntervalTx startSlot endSlot = do
    constr <- get
    let res  = txConstructorResult constr
    put constr { txConstructorResult = res <&&> Just (mempty, mustValidateInSlotRange $ interval startSlot endSlot) }

postValidatorTx :: Address -> Versioned Validator -> Maybe (TxOutDatum Datum) -> P.Value -> TransactionBuilder ()
postValidatorTx addr vld dat val = do
    constr <- get
    let res     = txConstructorResult constr
        hash    = validatorHash vld
        c       = mustPayToAddressWithReferenceValidator addr hash dat val
        lookups = otherScript vld
    put constr { txConstructorResult = res <&&> Just (lookups, c)}

postMintingPolicyTx :: Address -> Versioned MintingPolicy -> Maybe (TxOutDatum Datum) -> P.Value -> TransactionBuilder ()
postMintingPolicyTx addr mp dat val = do
    constr <- get
    let res     = txConstructorResult constr
        hash    = mintingPolicyHash mp
        c       = mustPayToAddressWithReferenceMintingPolicy addr hash dat val
        lookups = mintingPolicy mp
    put constr { txConstructorResult = res <&&> Just (lookups, c)}

datumTx :: ToData a => a -> TransactionBuilder ()
datumTx a = do
    constr <- get
    let res = txConstructorResult constr
        dat = Datum $ toBuiltinData a
    put constr { txConstructorResult = res <&&> Just (otherData dat, mempty) }

mustBeSignedByTx :: PaymentPubKeyHash -> TransactionBuilder ()
mustBeSignedByTx pkh = do
    constr <- get
    let res = txConstructorResult constr
    put constr { txConstructorResult = res <&&> Just (mempty, mustBeSignedBy pkh) }