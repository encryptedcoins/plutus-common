{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module PlutusAppsExtra.IO.Blockfrost where

import           Cardano.Api                      (NetworkId (..), NetworkMagic (..), StakeAddress, TxId, makeStakeAddress)
import           Cardano.Api.Shelley              (PoolId)
import           Control.Applicative              ((<|>))
import           Control.Exception                (throw)
import           Control.Monad                    (foldM)
import           Control.Monad.Catch              (Exception (..), MonadThrow (..))
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans              (MonadTrans (lift))
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Data.Aeson                       (eitherDecodeFileStrict)
import           Data.Data                        (Proxy (..))
import           Data.Foldable                    (find)
import           Data.Functor                     ((<&>))
import           Data.List                        (intersect, (\\))
import           Data.Maybe                       (listToMaybe)
import           Data.Text                        (Text)
import           Ledger                           (Address, AssetClass, CurrencySymbol, StakePubKeyHash (..), TokenName)
import           Ledger.Value                     (AssetClass (..), valueOf)
import           Network.HTTP.Client              (HttpException (..), newManager)
import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           PlutusAppsExtra.Types.Error      (BlockfrostError (UnknownNetworkMagic), ConnectionError (..))
import           PlutusAppsExtra.Utils.Address    (spkhToStakeCredential)
import           PlutusAppsExtra.Utils.Blockfrost (AccDelegationHistoryResponse (..), AssetHistoryResponse, AssetTxsResponse (..),
                                                   Bf (..), BfOrder (..), TxDelegationsCertsResponse, TxUTxoResponseOutput (..),
                                                   TxUtxoResponse (..), TxUtxoResponseInput (..))
import           Servant.API                      (Capture, Get, Header, JSON, QueryParam, (:<|>) ((:<|>)), (:>))
import           Servant.Client                   (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import qualified Servant.Client                   as Servant

class (Monad m, MonadIO m, MonadThrow m) => HasBlockfrost m where
    getBfToken   :: m BfToken
    getNetworkId :: m NetworkId

instance (MonadTrans t, Monad (t m), MonadIO (t m), MonadThrow (t m), HasBlockfrost m) => HasBlockfrost (t m) where
    getBfToken   = lift getBfToken
    getNetworkId = lift getNetworkId

tokenFilePath :: FilePath
tokenFilePath = "blockfrost.token"

portBf :: Int
portBf = 80

getAddressFromStakePubKeyHash :: HasBlockfrost m => PoolId -> StakePubKeyHash -> m (Maybe Address)
getAddressFromStakePubKeyHash poolId spkh = do
        runMaybeT $ do
            net     <- getNetworkId
            history <- MaybeT $ sequence $ getAccountDelegationHistory . makeStakeAddress net <$> spkhToStakeCredential spkh
            txHash  <- MaybeT $ pure $ adhrTxHash <$> find ((== poolId) . adhrPoolId) history
            MaybeT $ fmap turiAddress . listToMaybe . turInputs <$> getTxUtxo txHash

getStakeAddressLastPool :: HasBlockfrost m => StakeAddress -> m (Maybe PoolId)
getStakeAddressLastPool stakeAddr = fmap adhrPoolId . listToMaybe <$> getAccountDelegationHistory stakeAddr

getAddressFromStakeAddress :: HasBlockfrost m => StakeAddress -> m (Maybe Address)
getAddressFromStakeAddress stakeAddr = do
    txId <- fmap adhrTxHash . listToMaybe <$> getAccountDelegationHistory stakeAddr
    maybe (pure Nothing) (fmap (fmap turiAddress . listToMaybe . turInputs) . getTxUtxo) txId

-- find tx id where address have minted specific amount of asset
verifyAsset :: HasBlockfrost m => CurrencySymbol -> TokenName -> Integer -> Address -> m (Maybe TxId)
verifyAsset cs token amount addr = do
    history <- getAssetTxs cs token
    foldM (\res (atrTxHash -> txId) -> (res <|>) <$> (getTxUtxo txId <&> findOutput txId . turOutputs)) Nothing history
    where
        findOutput txId outs = const (Just txId) =<< find (\o -> turoAddress o == addr && valueOf (turoAmount o) cs token == amount) outs

verifyAssetFast :: HasBlockfrost m 
    => CurrencySymbol
    -> TokenName
    -> [(Address, Integer)]
    -> Maybe ([(Address, Integer, TxId)] -> IO ()) -- Function to save intermidiate results
    -> m [Either Address (Address, Integer, Cardano.Api.TxId)]
verifyAssetFast cs token recepients saveIntermidiate = do
    history <- getAssetTxs cs token
    go recepients history
    where
        total = length recepients
        -- end of recepients list
        go [] _ = pure []

        -- end of token history
        go rs [] = pure $ map (Left . fst) rs

        go rs ((atrTxHash -> txId) : hs) = do
            pairs <- map (\o -> (turoAddress o, valueOf (turoAmount o) cs token)) . turOutputs <$> getTxUtxo txId
            let res = map (\(a,b) -> (a,b,txId)) $ pairs `intersect` rs
                currentCounter = total - length rs
            liftIO $ maybe (pure ()) ($ res) saveIntermidiate
            liftIO $ putStrLn $ show currentCounter <> "/" <> show total
            (map Right res <>) <$> go (rs \\ pairs) hs

--------------------------------------------------- Blockfrost API ---------------------------------------------------

getTxUtxo :: HasBlockfrost m => TxId -> m TxUtxoResponse
getTxUtxo txId = getFromEndpointBF $ withBfToken $ \t -> getBfTxUtxo t $ Bf txId

getAccountDelegationHistory :: HasBlockfrost m => StakeAddress -> m [AccDelegationHistoryResponse]
getAccountDelegationHistory addr = getFromEndpointBF $ withBfToken $ \t -> getBfAccDelegationHistory t (Bf addr) (Just Desc)

getAssetTxs :: HasBlockfrost m => CurrencySymbol -> TokenName -> m [AssetTxsResponse]
getAssetTxs cs name = go 1
    where go n = do
            res <- getFromEndpointBF $ withBfToken $ \t -> getBfAssetTxs t (Bf $ AssetClass (cs, name)) (Just n)
            case res of
                [] -> pure []
                xs -> (xs <>) <$> go (n + 1)

getAssetHistory :: HasBlockfrost m => CurrencySymbol -> TokenName -> m [AssetHistoryResponse]
getAssetHistory cs name = getFromEndpointBF $ withBfToken $ \t -> getBfAssetHistory t (Bf $ AssetClass (cs, name))

type BfToken = Maybe Text

withBfToken :: (BfToken -> ClientM a) -> ClientM a
withBfToken ma = liftIO (eitherDecodeFileStrict tokenFilePath) >>= either error (ma . Just)

getFromEndpointBF :: HasBlockfrost m => ClientM a -> m a
getFromEndpointBF endpoint = do
    manager <- liftIO $ newManager tlsManagerSettings
    net <- getNet
    responseOrError <- liftIO $ runClientM
        endpoint
        (mkClientEnv manager (BaseUrl Http ("cardano-" <> net <> ".blockfrost.io")  portBf ""))
    case responseOrError of
        Left (Servant.ConnectionError (fromException -> Just (HttpExceptionRequest r c)))
                       -> throwM (ConnectionError r c)
        Left err       -> throwM err
        Right response -> pure response
    where
        getNet = getNetworkId >>= \case
            Mainnet                  -> pure "mainnet"
            Testnet (NetworkMagic 1) -> pure "preprod"
            Testnet (NetworkMagic 2) -> pure "preview"
            Testnet m                -> throw $ UnknownNetworkMagic m

type BlockfrostAPI = "api" :> "v0" :>
    (    GetAccDelegationHistory
    :<|> GetTxDelegationCerts
    :<|> GetTxUtxo
    :<|> GetAssetTxs
    :<|> GetAssetHistory
    )

type Auth = Header "project_id" Text

type GetAccDelegationHistory
    = Auth :> "accounts" :> Capture "Stake address" (Bf StakeAddress) :> "delegations" :> QueryParam "order" BfOrder :> Get '[JSON] [AccDelegationHistoryResponse]
type GetTxDelegationCerts
    = Auth :> "txs" :> Capture "Tx hash" (Bf TxId) :> "delegations" :> Get '[JSON] TxDelegationsCertsResponse
type GetTxUtxo
    = Auth :> "txs" :> Capture "Tx hash" (Bf TxId) :> "utxos" :> Get '[JSON] TxUtxoResponse
type GetAssetTxs
    = Auth :> "assets" :> Capture "Policy id" (Bf AssetClass) :> "transactions" :> QueryParam "page" Int :> Get '[JSON] [AssetTxsResponse]
type GetAssetHistory
    = Auth :> "assets" :> Capture "Policy id" (Bf AssetClass) :> "history" :> Get '[JSON] [AssetHistoryResponse]

getBfAccDelegationHistory :: BfToken -> Bf StakeAddress -> Maybe BfOrder -> ClientM [AccDelegationHistoryResponse]
getBfTxDelegationCerts    :: BfToken -> Bf TxId                          -> ClientM TxDelegationsCertsResponse
getBfTxUtxo               :: BfToken -> Bf TxId                          -> ClientM TxUtxoResponse
getBfAssetTxs             :: BfToken -> Bf AssetClass   -> Maybe Int     -> ClientM [AssetTxsResponse]
getBfAssetHistory         :: BfToken -> Bf AssetClass                    -> ClientM [AssetHistoryResponse]

(getBfAccDelegationHistory, getBfTxDelegationCerts, getBfTxUtxo, getBfAssetTxs, getBfAssetHistory)
    = (getBfAccDelegationHistory_, getBfTxDelegationCerts_, getBfTxUtxo_, getBfAssetTxs_, getBfAssetHistory_)
    where
        getBfAccDelegationHistory_
            :<|> getBfTxDelegationCerts_
            :<|> getBfTxUtxo_
            :<|> getBfAssetTxs_
            :<|> getBfAssetHistory_ = do
                client (Proxy @BlockfrostAPI)