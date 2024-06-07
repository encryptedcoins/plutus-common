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


import           Cardano.Api                         (NetworkId, NetworkMagic)
import qualified Cardano.Api                         as C
import           Cardano.Node.Emulator.Internal.Node (BalancingError, CardanoLedgerError)
import           Control.Exception                   (Exception, IOException)
import           Control.Monad.Catch                 (MonadThrow (..))
import qualified Data.Aeson                          as J
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           GHC.Generics                        (Generic)
import           Ledger                              (Address, CardanoTx, DecoratedTxOut, ToCardanoError)
import           Network.HTTP.Client                 (HttpExceptionContent, Request)
import           PlutusAppsExtra.IO.Tx.Internal      (TxState)
import           PlutusAppsExtra.PlutusApps          (ScriptLookups, TxConstraints)
import qualified PlutusAppsExtra.PlutusApps          as PlutusApps
import qualified PlutusLedgerApi.V1                  as PV1
import qualified PlutusLedgerApi.V3                  as P
import           Prelude

data ConnectionError = ConnectionError Request HttpExceptionContent
    deriving (Show, Exception)

instance J.ToJSON ConnectionError where
    toJSON _ = J.String "Connection error."

data TxBuilderError = TxBuilderError
    {
        txBuilderErrorIn     :: Text,
        txBuilderErrorReason :: Text
    }
    deriving (Show, Exception, Eq, Generic, J.FromJSON, J.ToJSON)

data MkTxError
    = AllConstructorsFailed [TxBuilderError]
    | CantExtractHashFromCardanoTx CardanoTx
    | CantExtractKeyHashesFromAddress Address
    | ConvertApiSerialisedTxToCardanoTxError
    | NotEnoughFunds P.Value
    | FailedToSubmit TxState
    | UnbuildableTxOut DecoratedTxOut ToCardanoError
    | UnbuildableExportTx
    | UnbuildableUnbalancedTx Text Text
    -- ^ UnbuildableUnbalancedTx has text representation of tx lookups and constraints because they dont have Eq and JSON instances.
    | UnparsableMetadata Text
    deriving (Show, Exception, Eq, Generic, J.FromJSON, J.ToJSON)

mkUnbuildableUnbalancedTxError :: (Show c1, Show c2) => ScriptLookups l -> TxConstraints c1 c2 -> MkTxError
mkUnbuildableUnbalancedTxError lookups cons = UnbuildableUnbalancedTx (T.pack $ show lookups) (T.pack $ show cons)

data BalanceExternalTxError
    = MakeUnbalancedTxError      PlutusApps.MkTxError Text Text
    -- ^ MakeUnbalancedTxError has text representation of tx lookups and constraints because they dont have Eq and JSON instances.
    | NonBabbageEraChangeAddress Address
    | MakeUtxoProviderError      {-UnbalancedTx-} BalancingError
    | MakeAutoBalancedTxError    {-UnbalancedTx-} CardanoLedgerError
    | UtxoIndexConversionError ToCardanoError
    deriving (Show, Exception, Eq, Generic)

mkUnbalancedTxError :: (Show c1, Show c2) => ScriptLookups l -> TxConstraints c1 c2 -> PlutusApps.MkTxError -> BalanceExternalTxError
mkUnbalancedTxError lookups cons err = MakeUnbalancedTxError err (T.pack $ show lookups) (T.pack $ show cons)

data WalletError
    = RestoredWalletParsingError Text
    | UnparsableAddress Text
    | WalletIdDoesntHaveAnyAssociatedAddresses
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

data SubmitTxError
    = FailedSumbit C.TxValidationErrorInCardanoMode
    | NoConnectionToLocalNode IOException
    | AwaiTxMaxAttemptsExceeded PV1.TxId
    deriving (Show, Exception)

throwMaybe :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
throwMaybe e = maybe (throwM e) pure

throwEither :: (MonadThrow m, Exception e) => e -> Either b a -> m a
throwEither e = either (const $ throwM e) pure
