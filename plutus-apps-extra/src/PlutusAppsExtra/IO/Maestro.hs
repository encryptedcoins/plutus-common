{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings   #-}

module PlutusAppsExtra.IO.Maestro where

import           Cardano.Api                   (NetworkId (..))
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Ledger                        (PubKeyHash, TxId)
import           Plutus.V2.Ledger.Api          (CurrencySymbol, TokenName)
import qualified PlutusAppsExtra.Api.Maestro   as Api
import           PlutusAppsExtra.Utils.Maestro (AccountAddressesHoldingAssetsResponse (..), Cursor, HasCursor, TxDetailsResponse,
                                                getCursor, AssetMintsAndBurnsResponse)
import           PlutusAppsExtra.Utils.Servant (handle404Maybe)

getAccountAddressesHoldingAssets :: NetworkId -> CurrencySymbol -> TokenName -> IO (Map PubKeyHash Integer)
getAccountAddressesHoldingAssets network cs name = fmap (Map.fromList . concatMap (\AccountAddressesHoldingAssetsResponse{..} -> aaharData))
    $ foldPages $ Api.getAccountAddressesHoldingAssets network cs name

getAssetMintsAndBurns :: NetworkId -> CurrencySymbol -> TokenName -> IO [AssetMintsAndBurnsResponse]
getAssetMintsAndBurns network cs name = foldPages $ Api.getAssetMintsAndBurns network cs name

getTxDetails :: NetworkId -> TxId -> IO (Maybe TxDetailsResponse)
getTxDetails network = handle404Maybe . Api.getTxDetails network

foldPages :: HasCursor a => (Maybe Cursor -> IO a) -> IO [a]
foldPages f = go $ f Nothing
    where
        go getRes = do
            res <- getRes
            case getCursor res of
                Nothing -> pure [res]
                Just s -> (res :) <$> go (f $ Just s)