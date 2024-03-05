{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module PlutusAppsExtra.IO.Maestro
    ( getAccountAddressesHoldingAssets
    , getAssetMintsAndBurns
    , getTxDetails
    , getTxOutput
    , getTxState
    , getUtxosAtAddress
    , getValidatorByHash
    , Api.sumbitTx
    ) where

import           Data.Coerce                   (coerce)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Text                     (Text)
import           Ledger                        (PubKeyHash, ScriptHash (..), TxId, TxOutRef, Validator (..), ValidatorHash (..), Versioned)
import           Plutus.V2.Ledger.Api          (CurrencySymbol, TokenName)
import           PlutusAppsExtra.Api.Maestro   (MonadMaestro)
import qualified PlutusAppsExtra.Api.Maestro   as Api
import           PlutusAppsExtra.Utils.Maestro (AccountAddressesHoldingAssetsResponse (..), AssetMintsAndBurnsResponse, Cursor, HasCursor,
                                                ScriptByHashResponse (..), TxDetailsResponse, TxOutputResponse, TxStateResponse,
                                                UtxosAtAddressResponse, getCursor)
import           PlutusAppsExtra.Utils.Servant (handle404Maybe)

getAccountAddressesHoldingAssets :: MonadMaestro m => CurrencySymbol -> TokenName -> m (Map PubKeyHash Integer)
getAccountAddressesHoldingAssets cs name = fmap (Map.fromList . concatMap (\AccountAddressesHoldingAssetsResponse{..} -> aaharData))
    $ foldPages $ Api.getAccountAddressesHoldingAssets cs name

getAssetMintsAndBurns :: MonadMaestro m => CurrencySymbol -> TokenName -> m [AssetMintsAndBurnsResponse]
getAssetMintsAndBurns cs name = foldPages $ Api.getAssetMintsAndBurns cs name

getTxDetails :: MonadMaestro m => TxId -> m (Maybe TxDetailsResponse)
getTxDetails = handle404Maybe . Api.getTxDetails

getTxOutput :: MonadMaestro m => TxOutRef -> m (Maybe TxOutputResponse)
getTxOutput = handle404Maybe . Api.getTxOutput

getTxState :: MonadMaestro m => TxId -> m (Maybe TxStateResponse)
getTxState = handle404Maybe . Api.getTxState

getUtxosAtAddress :: MonadMaestro m => Text -> Bool -> m [UtxosAtAddressResponse]
getUtxosAtAddress addrBech32 resolveDatums = foldPages $ Api.getUtxosAtAddress addrBech32 resolveDatums

getValidatorByHash :: MonadMaestro m => ValidatorHash -> m (Maybe (Versioned Validator))
getValidatorByHash (ValidatorHash hash) = fmap (fmap (coerce . sbhrScript)) $ handle404Maybe $ Api.getScriptByHash (coerce hash)

foldPages :: (HasCursor a, MonadMaestro m) => (Maybe Cursor -> m a) -> m [a]
foldPages f = go $ f Nothing
    where
        go getRes = do
            res <- getRes
            case getCursor res of
                Nothing -> pure [res]
                Just s  -> (res :) <$> go (f $ Just s)
