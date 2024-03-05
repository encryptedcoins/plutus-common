{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.IO.Wallet.Cardano where

import qualified Cardano.Wallet.Api.Client              as Client
import           Cardano.Wallet.Api.Types               (ApiNetworkInformation, ApiT (..), ApiWallet)
import           Cardano.Wallet.Primitive.Types         (WalletId)
import           Cardano.Wallet.Primitive.Types.Address (AddressState (..))
import           Control.Lens                           ((^.))
import           Control.Monad.Catch                    (MonadThrow (..))
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.Aeson.Lens                        (_String, key)
import           Data.Maybe                             (mapMaybe)
import           Data.Text                              (Text)
import           Data.Text.Class                        (FromText (fromText))
import           Ledger                                 (Address)
import           Network.HTTP.Client                    (HttpExceptionContent, Request)
import           Prelude                                hiding ((-))

import           PlutusAppsExtra.IO.Wallet.Internal     (HasWallet (..), getWalletId)
import           PlutusAppsExtra.Types.Error            (ConnectionError, WalletError (..))
import           PlutusAppsExtra.Utils.Address          (bech32ToAddress)
import           PlutusAppsExtra.Utils.Servant          (Endpoint, getFromEndpointOnPort, pattern ConnectionErrorOnPort)

------------------------------------------- Wallet functions -------------------------------------------

getFromEndpointWallet :: Endpoint a
getFromEndpointWallet = getFromEndpointOnPort 8090

pattern CardanoWalletApiConnectionError :: Request -> HttpExceptionContent -> ConnectionError
pattern CardanoWalletApiConnectionError req content <- ConnectionErrorOnPort 8090 req content

getWalletAddrBech32 :: HasWallet m => m Text
getWalletAddrBech32 = do
    walletId <- getWalletId
    getFromEndpointWallet (Client.listAddresses  Client.addressClient (ApiT walletId) (Just $ ApiT Unused)) >>= \case
        v:_ -> pure $ v ^. key "id"._String
        _   -> throwM $ WalletIdDoesntHaveAnyAssociatedAddresses walletId

getWalletAddr :: HasWallet m => m Address
getWalletAddr = do
    addrWalletBech32 <- getWalletAddrBech32
    case bech32ToAddress <$> fromText addrWalletBech32 of
        Right (Just addr) -> pure addr
        _                 -> throwM $ UnparsableAddress addrWalletBech32

ownAddresses :: HasWallet m => Maybe AddressState -> m [Address]
ownAddresses addrState = mapMaybe bech32ToAddress <$> ownAddressesBech32 addrState

ownAddressesBech32 :: HasWallet m => Maybe AddressState -> m [Text]
ownAddressesBech32 aState = do
    walletId <- getWalletId
    as <- getFromEndpointWallet $ Client.listAddresses Client.addressClient (ApiT walletId) (ApiT <$> aState)
    pure $ map (^. key "id"._String) as

getWalletFromId :: HasWallet m => WalletId -> m ApiWallet
getWalletFromId = getFromEndpointWallet . Client.getWallet Client.walletClient . ApiT

--------------------------------------------- Misc ---------------------------------------------

getHealth :: MonadIO m => m ApiNetworkInformation
getHealth = getFromEndpointWallet $ Client.networkInformation Client.networkClient
