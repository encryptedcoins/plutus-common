{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module PlutusAppsExtra.IO.ChainIndex.Maestro where

import           Control.Monad                    (join)
import           Control.Monad.Catch              (MonadThrow (..))
import           Data.Coerce                      (coerce)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import qualified Data.Set                         as Set
import           Ledger                           (Address, DecoratedTxOut (..), TxOutRef (..), Validator (..), Versioned (..),
                                                   addressStakingCredential)
import           Ledger.Scripts                   (ValidatorHash (..))
import           PlutusAppsExtra.Api.Maestro      (MonadMaestro (..), getScriptByHash)
import           PlutusAppsExtra.IO.Maestro       (getTxOutput, getUtxosAtAddress)
import           PlutusAppsExtra.Types.Error      (MaestroError (..))
import           PlutusAppsExtra.Types.Tx         (UtxoRequirement (..), UtxoRequirements)
import           PlutusAppsExtra.Utils.Address    (addressToBech32)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import           PlutusAppsExtra.Utils.Maestro    (ScriptByHashResponse (..), TxOutputResponse (..), UtxosAtAddressData (..),
                                                   UtxosAtAddressResponse (..))
import           PlutusAppsExtra.Utils.Network    (HasNetworkId (..))
import           PlutusLedgerApi.V1               (ScriptHash (..))
import           PlutusLedgerApi.V3               (Credential (..), addressCredential)

-- Get all unspent utxos at a given address
getUtxosAt :: MonadMaestro m => UtxoRequirements -> Address -> m MapUTXO
getUtxosAt reqs addr = do
    networkId <- getNetworkId
    addrBech32 <- maybe (throwM $ MaestroUnconvertableAddress addr) pure $ addressToBech32 networkId addr
    utxosAtAddressResponses <- getUtxosAtAddress addrBech32 (RequiresDatum `Set.member` reqs)
    _decoratedTxOutValidator <- case (addressCredential addr, RequiresValidator `Set.member` reqs) of
        (ScriptCredential sh, True) -> fmap (Just . sciptToValidator) $ sbhrScript <$> getScriptByHash sh
        _                           -> pure Nothing
    let evaluateTxOut UtxosAtAddressData{..} = do
            let _decoratedTxOutStakingCredential = addressStakingCredential addr
                _decoratedTxOutValue             = uaadValue
                _decoratedTxOutPubKeyDatum       = uaadDatum
                _decoratedTxOutReferenceScript   = uaadReferenceScript
            case (addressCredential addr, _decoratedTxOutPubKeyDatum) of
        
                (PubKeyCredential _decoratedTxOutPubKeyHash, _) 
                    -> pure $ Just PublicKeyDecoratedTxOut{..}
        
                (ScriptCredential (coerce -> _decoratedTxOutValidatorHash), Just _decoratedTxOutScriptDatum)
                    -> pure $ Just ScriptDecoratedTxOut{..}

                _   -> pure Nothing
    
        evaluateUtxo UtxosAtAddressData{..} = do
            txOut <- evaluateTxOut UtxosAtAddressData{..}
            pure (TxOutRef uaadTxHash uaadIndex, txOut)
    fmap (Map.fromList . catMaybes) $ mapM (fmap sequence . evaluateUtxo) $ concatMap uaarData utxosAtAddressResponses
  where
    sciptToValidator (Versioned script lang) = Versioned (Validator script) lang


getUnspentTxOutFromRef :: MonadMaestro m => UtxoRequirements -> TxOutRef -> m (Maybe DecoratedTxOut)
getUnspentTxOutFromRef reqs ref = do
    mbResponse <- getTxOutput ref
    join <$> mapM evaluateTxOut mbResponse
  where
    evaluateTxOut TxOutputResponse{..} = do
        let _decoratedTxOutStakingCredential = addressStakingCredential torAddress
            _decoratedTxOutValue             = torValue
            _decoratedTxOutPubKeyDatum       = torDatum
            _decoratedTxOutReferenceScript   = torReferenceScript
        _decoratedTxOutValidator <- case (addressCredential torAddress, RequiresValidator `Set.member` reqs) of
                (ScriptCredential sh, True) -> fmap (Just . sciptToValidator) $ sbhrScript <$> getScriptByHash sh
                _                           -> pure Nothing
        case (addressCredential torAddress, _decoratedTxOutPubKeyDatum) of

            (PubKeyCredential _decoratedTxOutPubKeyHash, _)                                  
                -> pure $ Just PublicKeyDecoratedTxOut{..}
    
            (ScriptCredential (coerce -> _decoratedTxOutValidatorHash), Just _decoratedTxOutScriptDatum) 
                -> pure $ Just ScriptDecoratedTxOut{..}
    
            _   -> pure Nothing

    sciptToValidator (Versioned script lang) = Versioned (Validator script) lang
