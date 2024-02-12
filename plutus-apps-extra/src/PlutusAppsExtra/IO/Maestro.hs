{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PlutusAppsExtra.IO.Maestro where

import           Cardano.Api                   (NetworkId (..))
import           Data.Coerce                   (coerce)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Text                     (Text)
import           Ledger                        (PubKeyHash, ScriptHash (..), TxId, TxOutRef, Validator (..), ValidatorHash (..), Versioned)
import           Plutus.V2.Ledger.Api          (CurrencySymbol, TokenName)
import qualified PlutusAppsExtra.Api.Maestro   as Api
import           PlutusAppsExtra.Utils.Maestro (AccountAddressesHoldingAssetsResponse (..), AssetMintsAndBurnsResponse, Cursor, HasCursor,
                                                ScriptByHashResponse (..), TxDetailsResponse, TxOutputResponse, UtxosAtAddressResponse,
                                                getCursor)
import           PlutusAppsExtra.Utils.Servant (handle404Maybe)

getAccountAddressesHoldingAssets :: NetworkId -> CurrencySymbol -> TokenName -> IO (Map PubKeyHash Integer)
getAccountAddressesHoldingAssets network cs name = fmap (Map.fromList . concatMap (\AccountAddressesHoldingAssetsResponse{..} -> aaharData))
    $ foldPages $ Api.getAccountAddressesHoldingAssets network cs name

getAssetMintsAndBurns :: NetworkId -> CurrencySymbol -> TokenName -> IO [AssetMintsAndBurnsResponse]
getAssetMintsAndBurns network cs name = foldPages $ Api.getAssetMintsAndBurns network cs name

getTxDetails :: NetworkId -> TxId -> IO (Maybe TxDetailsResponse)
getTxDetails network = handle404Maybe . Api.getTxDetails network

getTxOutput ::  NetworkId -> TxOutRef -> IO (Maybe TxOutputResponse)
getTxOutput network = handle404Maybe . Api.getTxOutput network

getUtxosAtAddress :: NetworkId -> Text -> Bool -> IO [UtxosAtAddressResponse]
getUtxosAtAddress network addrBech32 resolveDatums = foldPages $ Api.getUtxosAtAddress network addrBech32 resolveDatums

getValidatorByHash :: NetworkId -> ValidatorHash -> IO (Maybe (Versioned Validator))
getValidatorByHash network (ValidatorHash hash) = fmap (fmap (coerce . sbhrScript)) $ handle404Maybe $ Api.getScriptByHash network (coerce hash)

foldPages :: HasCursor a => (Maybe Cursor -> IO a) -> IO [a]
foldPages f = go $ f Nothing
    where
        go getRes = do
            res <- getRes
            case getCursor res of
                Nothing -> pure [res]
                Just s -> (res :) <$> go (f $ Just s)