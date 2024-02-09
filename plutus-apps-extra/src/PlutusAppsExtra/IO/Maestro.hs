{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PlutusAppsExtra.IO.Maestro where

import           Cardano.Api                   (NetworkId (..))
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Ledger                        (PubKeyHash, TxId)
import           Plutus.V2.Ledger.Api          (CurrencySymbol, TokenName)
import qualified PlutusAppsExtra.Api.Maestro   as Api
import           PlutusAppsExtra.Utils.Maestro (AccountAddressesHoldingAssetsResponse (..),
                                                AssetMintsAndBurnsResponse,
                                                Cursor, HasCursor,
                                                TxDetailsResponse, getCursor)
import           PlutusAppsExtra.Utils.Servant (handle404Maybe)

getAccountAddressesHoldingAssets :: MonadIO m
  => NetworkId
  -> CurrencySymbol
  -> TokenName
  -> m (Map PubKeyHash Integer)
getAccountAddressesHoldingAssets network cs name =
  fmap (Map.fromList . concatMap (\AccountAddressesHoldingAssetsResponse{..} -> aaharData))
    $ liftIO $ foldPages $ Api.getAccountAddressesHoldingAssets network cs name

getAssetMintsAndBurns :: MonadIO m
  => NetworkId
  -> CurrencySymbol
  -> TokenName
  -> m [AssetMintsAndBurnsResponse]
getAssetMintsAndBurns network cs name = liftIO $ foldPages $ Api.getAssetMintsAndBurns network cs name

getTxDetails :: MonadIO m
  => NetworkId
  -> TxId
  -> m (Maybe TxDetailsResponse)
getTxDetails network = liftIO . handle404Maybe . Api.getTxDetails network

foldPages :: HasCursor a => (Maybe Cursor -> IO a) -> IO [a]
foldPages f = go $ f Nothing
    where
        go getRes = do
            res <- getRes
            case getCursor res of
                Nothing -> pure [res]
                Just s  -> (res :) <$> go (f $ Just s)
