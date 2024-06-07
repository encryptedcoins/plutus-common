{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}

module PlutusAppsExtra.IO.Maestro
    ( getAccountAddressesHoldingAssets
    , getAssetMintsAndBurns
    , getTxDetails
    , getTxOutput
    , getTxState
    , getUtxosAtAddress
    , getValidatorByHash
    , Api.submitTx
    ) where

import           Data.Coerce                   (coerce)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Text                     (Text)
import           Ledger                        (PubKeyHash, ScriptHash (..), TxOutRef, Validator (..), ValidatorHash (..), Versioned)
import           PlutusAppsExtra.Api.Maestro   (MonadMaestro)
import qualified PlutusAppsExtra.Api.Maestro   as Api
import           PlutusAppsExtra.Utils.Maestro (AccountAddressesHoldingAssetsResponse (..), AssetMintsAndBurnsResponse, Cursor, HasCursor,
                                                ScriptByHashResponse (..), TxDetailsResponse, TxOutputResponse, TxStateResponse,
                                                UtxosAtAddressResponse, getCursor)
import           PlutusAppsExtra.Utils.Servant (handle404Maybe)
import qualified PlutusLedgerApi.V1            as PV1
import           PlutusLedgerApi.V3            (CurrencySymbol, TokenName)

getAccountAddressesHoldingAssets :: MonadMaestro m => CurrencySymbol -> TokenName -> m (Map PubKeyHash Integer)
getAccountAddressesHoldingAssets cs name = fmap (Map.fromList . concatMap (\AccountAddressesHoldingAssetsResponse{..} -> aaharData))
    $ foldPages $ Api.getAccountAddressesHoldingAssets cs name

getAssetMintsAndBurns :: MonadMaestro m => CurrencySymbol -> TokenName -> m [AssetMintsAndBurnsResponse]
getAssetMintsAndBurns cs name = foldPages $ Api.getAssetMintsAndBurns cs name

getTxDetails :: MonadMaestro m => PV1.TxId -> m (Maybe TxDetailsResponse)
getTxDetails = handle404Maybe . Api.getTxDetails

getTxOutput :: MonadMaestro m => TxOutRef -> m (Maybe TxOutputResponse)
getTxOutput = handle404Maybe . Api.getTxOutput

getTxState :: MonadMaestro m => PV1.TxId -> m (Maybe TxStateResponse)
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
