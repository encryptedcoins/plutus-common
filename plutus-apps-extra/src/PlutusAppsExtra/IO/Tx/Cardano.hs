{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.IO.Tx.Cardano where

import           Cardano.Node.Emulator                    (Params)
import qualified Cardano.Wallet.Api.Client                as Client
import           Cardano.Wallet.Api.Types                 (ApiSerialisedTransaction (..),
                                                           ApiSignTransactionPostData (ApiSignTransactionPostData), ApiT (..), ApiTxId (..))
import           Cardano.Wallet.Api.Types.SchemaMetadata  (TxMetadataSchema (..))
import           Cardano.Wallet.Primitive.Passphrase      (Passphrase (..))
import           Control.Concurrent                       (threadDelay)
import           Control.Lens                             ((^?))
import           Control.Monad                            (unless, void)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Data.Aeson                               (ToJSON (..))
import           Data.Aeson.Lens                          (_String, key)
import           Data.Coerce                              (coerce)
import qualified Data.Text                                as T
import           Data.Text.Class                          (FromText (fromText))
import           Ledger                                   (CardanoTx (..))
import           Ledger.Tx                                (getCardanoTxId)
import           Ledger.Typed.Scripts                     (ValidatorTypes (..))
import           PlutusTx.IsData                          (FromData, ToData)
import           Prelude                                  hiding ((-))

import           Cardano.Api                              (BabbageEra, TxMetadataInEra)
import           PlutusAppsExtra.PlutusApps               (ScriptLookups, TxConstraints, UnbalancedTx (..), mkTxWithParams)
import           PlutusAppsExtra.IO.Wallet                (HasWallet, getPassphrase, getWalletId)
import           PlutusAppsExtra.IO.Wallet.Cardano        (getFromEndpointWallet)
import           PlutusAppsExtra.Types.Error              (MkTxError (..), mkUnbuildableUnbalancedTxError, throwEither, throwMaybe)
import           PlutusAppsExtra.Utils.Tx                 (addMetadataToCardanoBuildTx, apiSerializedTxToCardanoTx, cardanoTxToSealedTx)

------------------------------------------- Tx functions -------------------------------------------

signTx :: HasWallet m => CardanoTx -> m CardanoTx
signTx ctx = do
    ppUser   <- getPassphrase
    walletId <- getWalletId
    asTx     <- sign walletId (coerce ppUser)
    throwMaybe (ConvertApiSerialisedTxToCardanoTxError asTx) $ apiSerializedTxToCardanoTx asTx
    where
        stx = cardanoTxToSealedTx ctx
        sign walletId pp = getFromEndpointWallet $ Client.signTransaction Client.transactionClient
            (ApiT walletId)
            (ApiSignTransactionPostData (ApiT stx) (ApiT pp))

-- Send a balanced transaction to Cardano Wallet Backend and return immediately
submitTx :: HasWallet m => CardanoTx -> m ()
submitTx ctx = do
    let stx = cardanoTxToSealedTx ctx
    walletId <- getWalletId
    void $ getFromEndpointWallet $
        Client.submitTransaction Client.transactionClient
            (ApiT walletId)
            (ApiSerialisedTransaction $ ApiT stx)

-- Wait until a transaction is confirmed (added to the ledger).
-- If the transaction is never added to the ledger then 'awaitTxConfirmed' never
-- returns
awaitTxConfirmed :: HasWallet m => CardanoTx -> m ()
awaitTxConfirmed ctx = go
    where
        go = do
            walletId <- getWalletId
            hash <- throwEither (CantExtractHashFromCardanoTx ctx) . fromText . T.pack . show  $ getCardanoTxId ctx
            res <- getFromEndpointWallet $ Client.getTransaction Client.transactionClient
                (ApiT walletId)
                (ApiTxId $ ApiT hash)
                TxMetadataNoSchema
            unless (confirmedResponse res) $ liftIO (threadDelay 1_000_000) >> go
        confirmedResponse res = case res ^? key "status"._String of
            Just "in_ledger" -> True
            _                -> False