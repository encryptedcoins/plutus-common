{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module PlutusAppsExtra.Api.Blockfrost where

import           Cardano.Api                      (NetworkId (..), NetworkMagic (..), StakeAddress)
import           Control.Exception                (throw)
import           Control.Monad.Catch              (Exception (..), MonadCatch, MonadThrow (..))
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Reader             (MonadReader, ReaderT (..), asks)
import qualified Data.ByteString                  as BS
import           Data.Data                        (Proxy (..))
import           Data.String                      (IsString (..))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Ledger                           (Datum, DatumHash, Script, ScriptHash, Validator, ValidatorHash, Versioned)
import           Network.HTTP.Client              (HttpException (..), newManager)
import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           Plutus.Script.Utils.Value        (AssetClass (..), CurrencySymbol, TokenName)
import           PlutusAppsExtra.Types.Error      (BlockfrostError (..), ConnectionError (..))
import           PlutusAppsExtra.Utils.Blockfrost (AccDelegationHistoryResponse (..), AccountAssociatedAddressesResponse,
                                                   AddressDetailsResponse, AssetAddressesResponse, AssetHistoryResponse,
                                                   AssetTxsResponse (..), Bf (..), BfAddress (..), BfOrder (..), TxUtxoResponse (..))
import           PlutusAppsExtra.Utils.Network    (HasNetworkId (..))
import           PlutusAppsExtra.Utils.Servant    (CBOR)
import qualified PlutusLedgerApi.V2               as PV2
import           Servant.API                      (Capture, Get, Header, JSON, PlainText, PostAccepted, QueryParam, ReqBody, (:>))
import qualified Servant.Client                   as Servant
import           Servant.Client                   (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)

--------------------------------------------------- Blockfrost API ---------------------------------------------------

type GetTxUtxo = ApiPrefix :> Auth :>
    "txs" :> Capture "Tx hash" (Bf PV2.TxId) :> "utxos" :> Get '[JSON] TxUtxoResponse

getTxUtxo :: MonadBlockfrost m => PV2.TxId -> m TxUtxoResponse
getTxUtxo txId = getFromEndpointBFWithToken $ \t ->
    client (Proxy @GetTxUtxo) t $ Bf txId

type GetAccountDelegationHistory = ApiPrefix :> Auth :>
    "accounts" :> Capture "Stake address" (Bf StakeAddress) :> "delegations" :> QueryParam "order" BfOrder :> Get '[JSON] [AccDelegationHistoryResponse]

getAccountDelegationHistory :: MonadBlockfrost m => StakeAddress -> m [AccDelegationHistoryResponse]
getAccountDelegationHistory addr = getFromEndpointBFWithToken $ \t ->
    client (Proxy @GetAccountDelegationHistory) t (Bf addr) (Just Desc)

type GetAddressDetails = ApiPrefix :> Auth :>
    "addresses" :> Capture "Address" BfAddress :> "total" :> Get '[JSON] AddressDetailsResponse

getAddressDetails :: MonadBlockfrost m =>  BfAddress -> m AddressDetailsResponse
getAddressDetails addr = getFromEndpointBFWithToken $ \t ->
        client (Proxy @GetAddressDetails) t addr

type GetAssetAddresses = ApiPrefix :> Auth :>
    "assets" :> Capture "Policy id" (Bf AssetClass) :> "addresses" :> QueryParam "order" BfOrder :> QueryParam "page" Int :> Get '[JSON] [AssetAddressesResponse]

getAssetAddresses :: MonadBlockfrost m =>  CurrencySymbol -> TokenName -> Maybe BfOrder -> Int -> m [AssetAddressesResponse]
getAssetAddresses cs name order page = getFromEndpointBFWithToken $ \t ->
    client (Proxy @GetAssetAddresses) t (Bf $ AssetClass (cs, name)) order (Just page)

type GetAssetTxs = ApiPrefix :> Auth :>
    "assets" :> Capture "Policy id" (Bf AssetClass) :> "transactions" :> QueryParam "order" BfOrder :> QueryParam "page" Int :> Get '[JSON] [AssetTxsResponse]

getAssetTxs :: MonadBlockfrost m =>  CurrencySymbol -> TokenName -> Maybe BfOrder -> Int -> m [AssetTxsResponse]
getAssetTxs cs name order page = getFromEndpointBFWithToken $ \t ->
    client (Proxy @GetAssetTxs) t (Bf $ AssetClass (cs, name)) order (Just page)

type GetAssetHistory = ApiPrefix :> Auth :>
    "assets" :> Capture "Policy id" (Bf AssetClass) :> "history" :> QueryParam "order" BfOrder :> QueryParam "page" Int :> Get '[JSON] [AssetHistoryResponse]

getAssetHistory :: MonadBlockfrost m =>  CurrencySymbol -> TokenName -> Maybe BfOrder -> Int -> m [AssetHistoryResponse]
getAssetHistory cs name order page = getFromEndpointBFWithToken $ \t ->
    client (Proxy @GetAssetHistory) t (Bf $ AssetClass (cs, name)) order (Just page)

type GetAccountAssociatedAddresses = ApiPrefix :> Auth :>
    "accounts" :> Capture "Stake address" (Bf StakeAddress) :> "addresses" :> QueryParam "page" Int :> QueryParam "order" BfOrder :> Get '[JSON] AccountAssociatedAddressesResponse

getAccountAssociatedAddresses :: MonadBlockfrost m => StakeAddress -> Int -> m AccountAssociatedAddressesResponse
getAccountAssociatedAddresses addr page = getFromEndpointBFWithToken $ \t ->
    client (Proxy @GetAccountAssociatedAddresses) t (Bf addr) (Just page) (Just Desc)

type GetDatumByHash = ApiPrefix :> Auth :>
    "txs" :> Capture "Datum hash" (Bf DatumHash) :> "utxos" :> Get '[JSON] Datum

getDatumByHashUnsafe :: MonadBlockfrost m =>  DatumHash -> m Datum
getDatumByHashUnsafe dh = getFromEndpointBFWithToken $ \t -> client (Proxy @GetDatumByHash) t (Bf dh)

type GetScriptByHash = ApiPrefix :> Auth :>
    "txs" :> Capture "Script hash" (Bf ScriptHash) :> "utxos" :> Get '[JSON] (Versioned Script)

getScriptByHashUnsafe :: MonadBlockfrost m => ScriptHash -> m (Versioned Script)
getScriptByHashUnsafe sh = getFromEndpointBFWithToken $ \t -> client (Proxy @GetScriptByHash) t (Bf sh)

type GetValidatorByHash = ApiPrefix :> Auth :>
    "txs" :> Capture "Datum hash" (Bf ValidatorHash) :> "utxos" :> Get '[JSON] (Versioned Validator)

getValidatorByHashUnsafe :: MonadBlockfrost m => ValidatorHash -> m (Versioned Validator)
getValidatorByHashUnsafe vh = getFromEndpointBFWithToken $ \t -> client (Proxy @GetValidatorByHash) t (Bf vh)

type SumbitTx = ApiPrefix :> Auth :>
    "tx" :> "submit" :> ReqBody '[CBOR] BS.ByteString :> PostAccepted '[PlainText] Text

submitTx :: MonadBlockfrost m => BS.ByteString -> m PV2.TxId
submitTx txCbor = fmap (fromString . T.unpack) . getFromEndpointBFWithToken $ \t -> client (Proxy @SumbitTx) t txCbor

------------------------------------------------------------- Helpers -------------------------------------------------------------

class (HasNetworkId m, MonadCatch m, MonadIO m) => MonadBlockfrost m where
    getBlockfrostToken :: m BlockfrostToken

type ApiPrefix = "api/v0"

type Auth = Header "project_id" Text

type BlockfrostToken = Maybe Text

getFromEndpointBFWithToken :: MonadBlockfrost m => (BlockfrostToken -> ClientM a) -> m a
getFromEndpointBFWithToken endpoint = getBlockfrostToken >>= getFromEndpointBF . endpoint

getFromEndpointBF :: MonadBlockfrost m => ClientM a -> m a
getFromEndpointBF endpoint = do
    manager <- liftIO $ newManager tlsManagerSettings
    bfNetwork <- mkBfNetwork <$> getNetworkId
    responseOrError <- liftIO $ runClientM
        endpoint
        (mkClientEnv manager (BaseUrl Http ("cardano-" <> bfNetwork <> ".blockfrost.io") portBf ""))
    case responseOrError of
        Left (Servant.ConnectionError (fromException -> Just (HttpExceptionRequest r c)))
                       -> throwM (ConnectionError r c)
        Left err       -> throwM err
        Right response -> pure response
    where
        mkBfNetwork = \case
            Mainnet                  -> "mainnet"
            Testnet (NetworkMagic 1) -> "preprod"
            Testnet (NetworkMagic 2) -> "preview"
            Testnet (NetworkMagic 4) -> "sanchonet"
            Testnet m                -> throw $ BlockfrostUnknownNetworkMagic m

portBf :: Int
portBf = 80

runBfMonad :: NetworkId -> BlockfrostToken -> RunBfMonad a -> IO a
runBfMonad networkId token = flip runReaderT (networkId, token) . unRunBfMonad

newtype RunBfMonad a = RunBfMonad {unRunBfMonad :: ReaderT (NetworkId, BlockfrostToken) IO a}
    deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO, MonadReader (NetworkId, BlockfrostToken))

instance HasNetworkId RunBfMonad where
    getNetworkId = asks fst

instance MonadBlockfrost RunBfMonad where
    getBlockfrostToken = asks snd
