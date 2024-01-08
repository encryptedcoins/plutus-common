{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module PlutusAppsExtra.Api.Maestro where

import           Cardano.Api                   (NetworkId (..), NetworkMagic (..))
import           Control.Exception             (Exception (fromException), throw)
import           Control.Monad                 (void)
import           Control.Monad.Catch           (MonadThrow (..))
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Aeson                    (eitherDecodeFileStrict)
import           Data.Data                     (Proxy (..))
import           Data.Text                     (Text)
import           Ledger                        (TxId (..))
import           Network.HTTP.Client           (HttpException (..), newManager)
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
import           Plutus.V1.Ledger.Value        (AssetClass (..), CurrencySymbol, TokenName)
import           PlutusAppsExtra.Types.Error   (ConnectionError (..), ExternalServiceError (..))
import           PlutusAppsExtra.Utils.Maestro (AccountAddressesHoldingAssetsResponse, Cursor, Maestro (..),
                                                TxDetailsResponse (..), AssetMintsAndBurnsResponse, EncodedAssetClass (EncodedAssetClass))
import           PlutusAppsExtra.Utils.Servant (CBOR)
import           Servant.API                   (Capture, Get, Header, JSON, NoContent, Post, QueryParam, ReqBody, (:>))
import           Servant.Client                (BaseUrl (BaseUrl), ClientM, Scheme (Http), client, mkClientEnv, runClientM)
import qualified Servant.Client                as Servant
import qualified Data.ByteString as BS

type GetAccountAddressesHoldingAssets = ApiPrefix :> Auth :>
    "assets" :> Capture "Asset" (Maestro AssetClass) :> "accounts" :>  QueryParam "cursor" Cursor :> Get '[JSON] AccountAddressesHoldingAssetsResponse

getAccountAddressesHoldingAssets :: NetworkId -> CurrencySymbol -> TokenName -> Maybe Cursor -> IO AccountAddressesHoldingAssetsResponse
getAccountAddressesHoldingAssets network cs name cursor = getFromEndpointMaestro network $ withMaestroToken $ \t ->
    client (Proxy @GetAccountAddressesHoldingAssets) t (Maestro $ AssetClass (cs, name)) cursor

type GetAssetMintsAndBurns = ApiPrefix :> Auth :>
    "assets" :> Capture "Asset" EncodedAssetClass :> "mints" :>  QueryParam "cursor" Cursor :> Get '[JSON] AssetMintsAndBurnsResponse

getAssetMintsAndBurns :: NetworkId -> CurrencySymbol -> TokenName -> Maybe Cursor -> IO AssetMintsAndBurnsResponse
getAssetMintsAndBurns network cs name cursor = getFromEndpointMaestro network $ withMaestroToken $ \t ->
    client (Proxy @GetAssetMintsAndBurns) t (EncodedAssetClass $ AssetClass (cs, name)) cursor

type GetTxDetails = ApiPrefix :> Auth :>
    "transactions" :> Capture "Tx hash" (Maestro TxId) :> Get '[JSON] TxDetailsResponse

getTxDetails :: NetworkId -> TxId -> IO TxDetailsResponse
getTxDetails network txId = getFromEndpointMaestro network $ withMaestroToken $ \t ->
    client (Proxy @GetTxDetails) t (Maestro txId)

type SumbitTx = ApiPrefix :> Auth :>
    "txmanager" :> ReqBody '[CBOR] BS.ByteString :> Post '[JSON] NoContent

sumbitTx :: NetworkId -> BS.ByteString -> IO ()
sumbitTx network txCbor = void $ getFromEndpointMaestro network $ withMaestroToken $ \t ->
    client (Proxy @SumbitTx) t txCbor

type ApiPrefix = "v1"

type Auth = Header "api-key" Text

------------------------------------------------------------- Helpers -------------------------------------------------------------

type MaestroToken = Maybe Text

withMaestroToken :: (MaestroToken -> ClientM a) -> ClientM a
withMaestroToken ma = liftIO (eitherDecodeFileStrict tokenFilePath) >>= either error (ma . Just)

getFromEndpointMaestro :: NetworkId -> ClientM a -> IO a
getFromEndpointMaestro network endpoint = do
    manager <- liftIO $ newManager tlsManagerSettings
    responseOrError <- liftIO $ runClientM
        endpoint
        (mkClientEnv manager (BaseUrl Http (maestroNetwork <> ".gomaestro-api.org") portMaestro ""))
    case responseOrError of
        Left (Servant.ConnectionError (fromException -> Just (HttpExceptionRequest r c)))
                       -> throwM (ConnectionError r c)
        Left err       -> throwM err
        Right response -> pure response
    where
        maestroNetwork = case network of
            Mainnet                  -> "mainnet"
            Testnet (NetworkMagic 1) -> "preprod"
            Testnet (NetworkMagic 2) -> "preview"
            Testnet m                -> throw $ MaestroUnknownNetworkMagic m

tokenFilePath :: FilePath
tokenFilePath = "maestro.token"

portMaestro :: Int
portMaestro = 80
