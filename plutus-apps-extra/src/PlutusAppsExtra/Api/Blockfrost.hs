{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module PlutusAppsExtra.Api.Blockfrost where

import           Cardano.Api                      (NetworkId (..), NetworkMagic (..), StakeAddress)
import           Control.Exception                (throw)
import           Control.Monad.Catch              (Exception (..), MonadThrow (..))
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Aeson                       (eitherDecodeFileStrict)
import           Data.Data                        (Proxy (..))
import           Data.Text                        (Text)
import           Ledger                           (Datum, DatumHash, Script, ScriptHash, TxId, Validator, ValidatorHash,
                                                   Versioned)
import           Network.HTTP.Client              (HttpException (..), newManager)
import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           Plutus.Script.Utils.Value        (AssetClass (..), CurrencySymbol, TokenName)
import           PlutusAppsExtra.Types.Error      (ConnectionError (..), ExternalServiceError (BlockfrostUnknownNetworkMagic))
import           PlutusAppsExtra.Utils.Blockfrost (AccDelegationHistoryResponse (..), AssetAddressesResponse,
                                                   AssetHistoryResponse, AssetTxsResponse (..), Bf (..), BfOrder (..),
                                                   TxUtxoResponse (..))
import           Servant.API                      (Capture, Get, Header, JSON, QueryParam, (:>))
import           Servant.Client                   (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import qualified Servant.Client                   as Servant

--------------------------------------------------- Blockfrost API ---------------------------------------------------

type GetTxUtxo = ApiPrefix :> Auth :>
    "txs" :> Capture "Tx hash" (Bf TxId) :> "utxos" :> Get '[JSON] TxUtxoResponse

getTxUtxo :: NetworkId -> TxId -> IO TxUtxoResponse
getTxUtxo network txId = getFromEndpointBF network $ withBfToken $ \t ->
    client (Proxy @GetTxUtxo) t $ Bf txId

type GetAccountDelegationHistory = ApiPrefix :> Auth :>
    "accounts" :> Capture "Stake address" (Bf StakeAddress) :> "delegations" :> QueryParam "order" BfOrder :> Get '[JSON] [AccDelegationHistoryResponse]

getAccountDelegationHistory :: NetworkId -> StakeAddress -> IO [AccDelegationHistoryResponse]
getAccountDelegationHistory network addr = getFromEndpointBF network $ withBfToken $ \t ->
    client (Proxy @GetAccountDelegationHistory) t (Bf addr) (Just Desc)

type GetAssetAddresses = ApiPrefix :> Auth :>
    "assets" :> Capture "Policy id" (Bf AssetClass) :> "addresses" :> QueryParam "order" BfOrder :> QueryParam "page" Int :> Get '[JSON] [AssetAddressesResponse]

getAssetAddresses :: NetworkId -> CurrencySymbol -> TokenName -> Maybe BfOrder -> Int -> IO [AssetAddressesResponse]
getAssetAddresses network cs name order page = getFromEndpointBF network $ withBfToken $ \t ->
    client (Proxy @GetAssetAddresses) t (Bf $ AssetClass (cs, name)) order (Just page)

type GetAssetTxs = ApiPrefix :> Auth :>
    "assets" :> Capture "Policy id" (Bf AssetClass) :> "transactions" :> QueryParam "order" BfOrder :> QueryParam "page" Int :> Get '[JSON] [AssetTxsResponse]

getAssetTxs :: NetworkId -> CurrencySymbol -> TokenName -> Maybe BfOrder -> Int -> IO [AssetTxsResponse]
getAssetTxs network cs name order page = getFromEndpointBF network $ withBfToken $ \t ->
    client (Proxy @GetAssetTxs) t (Bf $ AssetClass (cs, name)) order (Just page)

type GetAssetHistory = ApiPrefix :> Auth :>
    "assets" :> Capture "Policy id" (Bf AssetClass) :> "history" :> Get '[JSON] [AssetHistoryResponse]

getAssetHistory :: NetworkId -> CurrencySymbol -> TokenName -> IO [AssetHistoryResponse]
getAssetHistory network cs name = getFromEndpointBF network $ withBfToken $ \t ->
    client (Proxy @GetAssetHistory) t (Bf $ AssetClass (cs, name))

type GetAccountAssociatedAddresses = ApiPrefix :> Auth :>
    "accounts" :> Capture "Stake address" (Bf StakeAddress) :> "addresses" :> QueryParam "page" Int :> QueryParam "order" BfOrder :> Get '[JSON] [Text]

getAccountAssociatedAddresses :: NetworkId -> StakeAddress -> Int -> IO [Text]
getAccountAssociatedAddresses network addr page = getFromEndpointBF network $ withBfToken $ \t ->
    client (Proxy @GetAccountAssociatedAddresses) t (Bf addr) (Just page) (Just Desc)

type GetDatumByHash = ApiPrefix :> Auth :>
    "txs" :> Capture "Datum hash" (Bf DatumHash) :> "utxos" :> Get '[JSON] Datum

getDatumByHashUnsafe :: NetworkId -> DatumHash -> IO Datum
getDatumByHashUnsafe network dh = getFromEndpointBF network $ withBfToken $ \t -> client (Proxy @GetDatumByHash) t (Bf dh)

type GetScriptByHash = ApiPrefix :> Auth :>
    "txs" :> Capture "Script hash" (Bf ScriptHash) :> "utxos" :> Get '[JSON] (Versioned Script)

getScriptByHashUnsafe :: NetworkId -> ScriptHash -> IO (Versioned Script)
getScriptByHashUnsafe network sh = getFromEndpointBF network $ withBfToken $ \t -> client (Proxy @GetScriptByHash) t (Bf sh)

type GetValidatorByHash = ApiPrefix :> Auth :>
    "txs" :> Capture "Datum hash" (Bf ValidatorHash) :> "utxos" :> Get '[JSON] (Versioned Validator)

getValidatorByHashUnsafe :: NetworkId -> ValidatorHash -> IO (Versioned Validator)
getValidatorByHashUnsafe network vh = getFromEndpointBF network $ withBfToken $ \t -> client (Proxy @GetValidatorByHash) t (Bf vh)

------------------------------------------------------------- Helpers -------------------------------------------------------------

type ApiPrefix = "api/v0"

type Auth = Header "project_id" Text

type BfToken = Maybe Text

withBfToken :: (BfToken -> ClientM a) -> ClientM a
withBfToken ma = liftIO (eitherDecodeFileStrict tokenFilePath) >>= either error (ma . Just)

getFromEndpointBF :: NetworkId -> ClientM a -> IO a
getFromEndpointBF network endpoint = do
    manager <- liftIO $ newManager tlsManagerSettings
    responseOrError <- liftIO $ runClientM
        endpoint
        (mkClientEnv manager (BaseUrl Http ("cardano-" <> bfNetwork <> ".blockfrost.io")  portBf ""))
    case responseOrError of
        Left (Servant.ConnectionError (fromException -> Just (HttpExceptionRequest r c)))
                       -> throwM (ConnectionError r c)
        Left err       -> throwM err
        Right response -> pure response
    where
        bfNetwork = case network of
            Mainnet                  -> "mainnet"
            Testnet (NetworkMagic 1) -> "preprod"
            Testnet (NetworkMagic 2) -> "preview"
            Testnet m                -> throw $ BlockfrostUnknownNetworkMagic m

tokenFilePath :: FilePath
tokenFilePath = "blockfrost.token"

portBf :: Int
portBf = 80