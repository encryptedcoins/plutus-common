{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module PlutusAppsExtra.Api.Maestro where

import           Cardano.Api                   (NetworkId (..), NetworkMagic (..))
import           Control.Exception             (Exception (fromException), throw)
import           Control.Monad.Catch           (MonadCatch, MonadThrow (..))
import           Control.Monad.IO.Class        (MonadIO (..))
import qualified Data.ByteString               as BS
import           Data.Data                     (Proxy (..))
import           Data.String                   (IsString (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Ledger                        (ScriptHash, TxId (..), TxOutRef (..))
import           Network.HTTP.Client           (HttpException (..), newManager)
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
import           Plutus.V1.Ledger.Value        (AssetClass (..), CurrencySymbol, TokenName)
import           PlutusAppsExtra.Types.Error   (ConnectionError (..), MaestroError (..))
import           PlutusAppsExtra.Utils.Maestro (AccountAddressesHoldingAssetsResponse, AssetMintsAndBurnsResponse, Cursor,
                                                EncodedAssetClass (EncodedAssetClass), Maestro (..), ScriptByHashResponse,
                                                TxDetailsResponse (..), TxHistoryResponse (..), TxOutputResponse, TxStateResponse,
                                                UtxosAtAddressResponse)
import           PlutusAppsExtra.Utils.Network (HasNetworkId (..))
import           PlutusAppsExtra.Utils.Servant (CBOR)
import           Servant.API                   (Capture, Get, Header, JSON, QueryParam, ReqBody, (:>))
import           Servant.API.ContentTypes      (PlainText)
import           Servant.API.Verbs             (PostAccepted)
import           Servant.Client                (BaseUrl (BaseUrl), ClientM, Scheme (Http), client, mkClientEnv, runClientM)
import qualified Servant.Client                as Servant

type GetUtxosAtAddress = ApiPrefix :> Auth :>
    "addresses" :> Capture "Address" Text :> "utxos" :> QueryParam "resolve-datums" Bool :> QueryParam "cursor" Cursor :> Get '[JSON] UtxosAtAddressResponse

getUtxosAtAddress :: MonadMaestro m =>  Text -> Bool -> Maybe Cursor -> m UtxosAtAddressResponse
getUtxosAtAddress addrBech32 resolveDatums cursor = getFromEndpointMaestroWithToken $ \t ->
    client (Proxy @GetUtxosAtAddress) t addrBech32 (Just resolveDatums) cursor

type GetAccountAddressesHoldingAssets = ApiPrefix :> Auth :>
    "assets" :> Capture "Asset" (Maestro AssetClass) :> "accounts" :>  QueryParam "cursor" Cursor :> Get '[JSON] AccountAddressesHoldingAssetsResponse

getAccountAddressesHoldingAssets :: MonadMaestro m => CurrencySymbol -> TokenName -> Maybe Cursor -> m AccountAddressesHoldingAssetsResponse
getAccountAddressesHoldingAssets cs name cursor = getFromEndpointMaestroWithToken $ \t ->
    client (Proxy @GetAccountAddressesHoldingAssets) t (Maestro $ AssetClass (cs, name)) cursor

type GetAssetMintsAndBurns = ApiPrefix :> Auth :>
    "assets" :> Capture "Asset" EncodedAssetClass :> "mints" :>  QueryParam "cursor" Cursor :> Get '[JSON] AssetMintsAndBurnsResponse

getAssetMintsAndBurns :: MonadMaestro m =>  CurrencySymbol -> TokenName -> Maybe Cursor -> m AssetMintsAndBurnsResponse
getAssetMintsAndBurns cs name cursor = getFromEndpointMaestroWithToken $ \t ->
    client (Proxy @GetAssetMintsAndBurns) t (EncodedAssetClass $ AssetClass (cs, name)) cursor

type GetScriptByHash = ApiPrefix :> Auth :>
    "scripts" :> Capture "Script hash" (Maestro ScriptHash) :> Get '[JSON] ScriptByHashResponse

getScriptByHash :: MonadMaestro m =>  ScriptHash -> m ScriptByHashResponse
getScriptByHash sh = getFromEndpointMaestroWithToken $ \t ->
    client (Proxy @GetScriptByHash) t (Maestro sh)

type GetTxDetails = ApiPrefix :> Auth :>
    "transactions" :> Capture "Tx hash" (Maestro TxId) :> Get '[JSON] TxDetailsResponse

getTxDetails :: MonadMaestro m =>  TxId -> m TxDetailsResponse
getTxDetails txId = getFromEndpointMaestroWithToken $ \t ->
    client (Proxy @GetTxDetails) t (Maestro txId)


type GetTxOutput = ApiPrefix :> Auth :>
    "transactions" :> Capture "Tx hash" (Maestro TxId) :> "outputs" :> Capture "Output index" Integer :> "txo" :>  Get '[JSON] TxOutputResponse

getTxOutput :: MonadMaestro m => TxOutRef -> m TxOutputResponse
getTxOutput (TxOutRef txId txIdIndex) = getFromEndpointMaestroWithToken $ \t ->
    client (Proxy @GetTxOutput) t (Maestro txId) txIdIndex

type GetTxHistory  = ApiPrefix :> Auth :>
    "txmanager" :> "history" :> Get '[JSON] TxHistoryResponse

getTxHistory :: MonadMaestro m => m TxHistoryResponse
getTxHistory = getFromEndpointMaestroWithToken $ \t ->
    client (Proxy @GetTxHistory) t

type GetTxState = ApiPrefix :> Auth :>
    "txmanager" :> Capture "Tx hash" (Maestro TxId) :> "state" :> Get '[JSON] TxStateResponse

getTxState :: MonadMaestro m => TxId -> m TxStateResponse
getTxState txId = getFromEndpointMaestroWithToken $ \t ->
    client (Proxy @GetTxState) t (Maestro txId)

type SumbitTx = ApiPrefix :> Auth :>
    "txmanager" :> ReqBody '[CBOR] BS.ByteString :> PostAccepted '[PlainText] Text

submitTx :: MonadMaestro m => BS.ByteString -> m TxId
submitTx txCbor = fmap (fromString . T.unpack) . getFromEndpointMaestroWithToken $ \t ->
        client (Proxy @SumbitTx) t txCbor

type ApiPrefix = "v1"

type Auth = Header "api-key" Text

------------------------------------------------------------- Helpers -------------------------------------------------------------

class (MonadCatch m, MonadIO m, HasNetworkId m) => MonadMaestro m where
    getMaestroToken :: m MaestroToken

type MaestroToken = Maybe Text

getFromEndpointMaestroWithToken :: MonadMaestro m => (MaestroToken -> ClientM a) -> m a
getFromEndpointMaestroWithToken endpoint = getMaestroToken >>= getFromEndpointMaestro . endpoint

getFromEndpointMaestro :: MonadMaestro m => ClientM a -> m a
getFromEndpointMaestro endpoint = do
    networkId <- getNetworkId
    manager <- liftIO $ newManager tlsManagerSettings
    responseOrError <- liftIO $ runClientM
        endpoint
        (mkClientEnv manager (BaseUrl Http (maestroNetwork networkId <> ".gomaestro-api.org") portMaestro ""))
    case responseOrError of
        Left (Servant.ConnectionError (fromException -> Just (HttpExceptionRequest r c)))
                       -> throwM (ConnectionError r c)
        Left err       -> throwM err
        Right response -> pure response
    where
        maestroNetwork = \case
            Mainnet                  -> "mainnet"
            Testnet (NetworkMagic 1) -> "preprod"
            Testnet (NetworkMagic 2) -> "preview"
            Testnet m                -> throw $ MaestroUnknownNetworkMagic m

portMaestro :: Int
portMaestro = 80