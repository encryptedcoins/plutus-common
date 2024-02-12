{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlutusAppsExtra.IO.ChainIndex.Maestro where

import           Control.Exception                (Exception)
import           Control.Monad                    (join)
import           Control.Monad.Catch              (MonadThrow (..))
import           Control.Monad.IO.Class           (MonadIO (..))
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import qualified Data.Set                         as Set
import           Ledger                           (Address, DecoratedTxOut (..), NetworkId, TxOutRef (..), addressStakingCredential)
import           Plutus.V1.Ledger.Api             (Credential (..), addressCredential)
import           PlutusAppsExtra.IO.Maestro       (getTxOutput, getUtxosAtAddress, getValidatorByHash)
import           PlutusAppsExtra.Types.Tx         (UtxoRequirement (..), UtxoRequirements)
import           PlutusAppsExtra.Utils.Address    (addressToBech32)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import           PlutusAppsExtra.Utils.Maestro    (TxOutputResponse (..), UtxosAtAddressData (..), UtxosAtAddressResponse (..))

class (MonadThrow m, MonadIO m) => MonadMaestro m where
    getNetworkId :: m NetworkId

-- Get all unspent utxos at a given address
getUtxosAt :: MonadMaestro m => UtxoRequirements -> Address -> m MapUTXO
getUtxosAt reqs addr = do
    networkId <- getNetworkId
    addrBech32 <- maybe (throwM $ UnconvertableAddress addr) pure $ addressToBech32 networkId addr
    utxosAtAddressResponses <- liftIO $ getUtxosAtAddress networkId addrBech32 (RequiresDatum `Set.member` reqs)
    _decoratedTxOutValidator <- case (addressCredential addr, RequiresValidator `Set.member` reqs) of
        (ScriptCredential vh, True) -> liftIO $ getValidatorByHash networkId vh
        _                           -> pure Nothing
    let evaluateTxOut UtxosAtAddressData{..} = do
            let _decoratedTxOutStakingCredential = addressStakingCredential addr
                _decoratedTxOutValue             = uaadValue
                _decoratedTxOutPubKeyDatum       = uaadDatum
                _decoratedTxOutReferenceScript   = uaadReferenceScript
            case (addressCredential addr, _decoratedTxOutPubKeyDatum) of
                (PubKeyCredential _decoratedTxOutPubKeyHash, _)                                  -> pure $ Just PublicKeyDecoratedTxOut{..}
                (ScriptCredential _decoratedTxOutValidatorHash, Just _decoratedTxOutScriptDatum) -> pure $ Just ScriptDecoratedTxOut{..}
                _ -> pure Nothing
        evaluateUtxo UtxosAtAddressData{..} = do
            txOut <- evaluateTxOut UtxosAtAddressData{..}
            pure (TxOutRef uaadTxHash uaadIndex, txOut)
    fmap (Map.fromList . catMaybes) $ mapM (fmap sequence . evaluateUtxo) $ concatMap uaarData utxosAtAddressResponses

getUnspentTxOutFromRef :: MonadMaestro m => UtxoRequirements -> TxOutRef -> m (Maybe DecoratedTxOut)
getUnspentTxOutFromRef reqs ref = do
        networkId <- getNetworkId
        mbResponse <- liftIO $ getTxOutput networkId ref
        join <$> mapM (evaluateTxOut networkId) mbResponse
    where
        evaluateTxOut networkId TxOutputResponse{..} = do
            let _decoratedTxOutStakingCredential = addressStakingCredential torAddress
                _decoratedTxOutValue             = torValue
                _decoratedTxOutPubKeyDatum       = torDatum
                _decoratedTxOutReferenceScript   = torReferenceScript
            _decoratedTxOutValidator <- case (addressCredential torAddress, RequiresValidator `Set.member` reqs) of
                    (ScriptCredential vh, True) -> liftIO $ getValidatorByHash networkId vh
                    _                           -> pure Nothing
            case (addressCredential torAddress, _decoratedTxOutPubKeyDatum) of
                (PubKeyCredential _decoratedTxOutPubKeyHash, _)                                  -> pure $ Just PublicKeyDecoratedTxOut{..}
                (ScriptCredential _decoratedTxOutValidatorHash, Just _decoratedTxOutScriptDatum) -> pure $ Just ScriptDecoratedTxOut{..}
                _                                                                                -> pure Nothing
newtype MasetroChainindexError
    = UnconvertableAddress Address
    deriving (Show, Exception)