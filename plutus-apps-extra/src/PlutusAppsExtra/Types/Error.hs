{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.Types.Error where

import           Cardano.Api                    (CardanoMode, FromJSON, NetworkMagic, ToJSON, TxValidationErrorInMode)
import           Cardano.Node.Emulator          (BalancingError, CardanoLedgerError)
import           Cardano.Wallet.Api.Types       (ApiSerialisedTransaction)
import           Cardano.Wallet.Primitive.Types (WalletId)
import           Control.Exception              (Exception)
import           Control.Monad.Catch            (MonadThrow (..))
import qualified Data.Aeson                     as J
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
import           Ledger                         (Address, CardanoTx, DecoratedTxOut, ToCardanoError)
import           Network.HTTP.Client            (HttpExceptionContent, Request)
import qualified Plutus.V2.Ledger.Api           as P
import           Prelude

data ConnectionError = ConnectionError Request HttpExceptionContent
    deriving (Show, Exception)

instance ToJSON ConnectionError where
    toJSON _ = J.String "Connection error."

data TxBuilderError = TxBuilderError
    {
        txBuilderErrorIn     :: Text,
        txBuilderErrorReason :: Text
    }
    deriving (Show, Exception, Eq, Generic, FromJSON, ToJSON)

data MkTxError 
    = AllConstructorsFailed [TxBuilderError]
    | CantExtractHashFromCardanoTx CardanoTx
    | CantExtractKeyHashesFromAddress Address
    -- | CantExtractTxOutRefsFromEmulatorTx
    | ConvertApiSerialisedTxToCardanoTxError ApiSerialisedTransaction
    -- | ConvertCardanoTxToSealedTxError CardanoTx
    | NotEnoughFunds P.Value
    | UnbuildableTxOut DecoratedTxOut ToCardanoError
    | UnbuildableExportTx
    | UnbuildableUnbalancedTx
    | UnparsableMetadata
    deriving (Show, Exception, Eq, Generic, FromJSON, ToJSON)

data BalanceExternalTxError 
    = MakeUnbalancedTxError
    | NonBabbageEraChangeAddress
    | MakeUtxoProviderError BalancingError
    | MakeAutoBalancedTxError CardanoLedgerError
    deriving (Show, Exception, Eq, Generic, FromJSON, ToJSON)

data WalletError
    = RestoredWalletParsingError Text
    | UnparsableAddress Text
    | WalletIdDoesntHaveAnyAssociatedAddresses WalletId
    | AddressDoesntCorrespondToPubKey Address
    deriving (Show, Exception)

data BlockfrostError
    = UnknownNetworkMagic NetworkMagic
    deriving (Show, Exception)

data SubmitTxToLocalNodeError
    = FailedSumbit (TxValidationErrorInMode CardanoMode)
    | NoConnectionToLocalNode
    deriving (Show, Exception)

throwMaybe :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
throwMaybe e = maybe (throwM e) pure 

throwEither :: (MonadThrow m, Exception e) => e -> Either b a -> m a
throwEither e = either (const $ throwM e) pure
