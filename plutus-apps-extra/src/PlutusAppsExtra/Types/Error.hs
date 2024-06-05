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

import           Cardano.Api                    (CardanoMode, FromJSON, NetworkId, NetworkMagic, ToJSON, TxValidationErrorInMode)
import           Cardano.Node.Emulator          (BalancingError, CardanoLedgerError)
import           Cardano.Wallet.Api.Types       (ApiSerialisedTransaction)
import           Cardano.Wallet.Primitive.Types (WalletId)
import           Control.Exception              (Exception, IOException)
import           Control.Monad.Catch            (MonadThrow (..))
import qualified Data.Aeson                     as J
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           GHC.Generics                   (Generic)
import           Ledger                         (Address, CardanoTx, DecoratedTxOut, ToCardanoError)
import           PlutusAppsExtra.PlutusApps     (TxConstraints (..), ScriptLookups (..), UnbalancedTx)
import qualified PlutusAppsExtra.PlutusApps     as PlutusApps
import           Network.HTTP.Client            (HttpExceptionContent, Request)
import qualified Plutus.V2.Ledger.Api           as P
import           PlutusAppsExtra.IO.Tx.Internal (TxState)
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
    | ConvertApiSerialisedTxToCardanoTxError ApiSerialisedTransaction
    | NotEnoughFunds P.Value
    | FailedToSubmit TxState
    | UnbuildableTxOut DecoratedTxOut ToCardanoError
    | UnbuildableExportTx UnbalancedTx
    | UnbuildableUnbalancedTx Text Text
    -- ^ UnbuildableUnbalancedTx has text representation of tx lookups and constraints because they dont have Eq and JSON instances.
    | UnparsableMetadata Text
    deriving (Show, Exception, Eq, Generic, FromJSON, ToJSON)

mkUnbuildableUnbalancedTxError :: (Show c1, Show c2) => ScriptLookups l -> TxConstraints c1 c2 -> MkTxError
mkUnbuildableUnbalancedTxError lookups cons = UnbuildableUnbalancedTx (T.pack $ show lookups) (T.pack $ show cons)

data BalanceExternalTxError
    = MakeUnbalancedTxError      PlutusApps.MkTxError Text Text
    -- ^ MakeUnbalancedTxError has text representation of tx lookups and constraints because they dont have Eq and JSON instances.
    | NonBabbageEraChangeAddress Address
    | MakeUtxoProviderError      UnbalancedTx BalancingError
    | MakeAutoBalancedTxError    UnbalancedTx CardanoLedgerError
    deriving (Show, Exception, Eq, Generic, FromJSON, ToJSON)

mkUnbalancedTxError :: (Show c1, Show c2) => ScriptLookups l -> TxConstraints c1 c2 -> PlutusApps.MkTxError -> BalanceExternalTxError
mkUnbalancedTxError lookups cons err = MakeUnbalancedTxError err (T.pack $ show lookups) (T.pack $ show cons)

data WalletError
    = RestoredWalletParsingError Text
    | UnparsableAddress Text
    | WalletIdDoesntHaveAnyAssociatedAddresses WalletId
    | AddressDoesntCorrespondToPubKey Address
    | UnbuildableStakeAddress Text
    deriving (Show, Exception)

data BlockfrostError
    = BlockfrostUnknownNetworkMagic NetworkMagic
    | BlockfrostAddressToBech32Error NetworkId Address
    | BlockfrostUnserialisableTx CardanoTx
    deriving (Show, Exception)

data MaestroError
    = MaestroNoTokenProvided
    | MaestroUnknownNetworkMagic NetworkMagic
    | MaestroUnconvertableAddress Address
    | MaestroUnserialisableTx CardanoTx
    deriving (Show, Exception)

data SubmitTxToLocalNodeError
    = FailedSumbit (TxValidationErrorInMode CardanoMode)
    | NoConnectionToLocalNode IOException
    deriving (Show, Exception)

throwMaybe :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
throwMaybe e = maybe (throwM e) pure

throwEither :: (MonadThrow m, Exception e) => e -> Either b a -> m a
throwEither e = either (const $ throwM e) pure