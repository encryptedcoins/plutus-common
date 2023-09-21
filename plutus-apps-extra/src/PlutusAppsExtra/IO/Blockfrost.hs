{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module PlutusAppsExtra.IO.Blockfrost where

import           Cardano.Api                      (NetworkId (..), NetworkMagic (..), StakeAddress, TxId, makeStakeAddress)
import           Cardano.Api.Shelley              (PoolId)
import           Control.Applicative              ((<|>))
import           Control.Exception                (throw)
import           Control.Lens                     ((^.))
import           Control.Lens.Tuple               (Field3 (_3))
import           Control.Monad                    (foldM)
import           Control.Monad.Catch              (Exception (..), MonadThrow (..))
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Data.Aeson                       (eitherDecodeFileStrict)
import           Data.Data                        (Proxy (..))
import           Data.Foldable                    (find)
import           Data.Functor                     ((<&>))
import           Data.List                        (intersect, (\\))
import           Data.Maybe                       (listToMaybe)
import           Data.Text                        (Text)
import           Ledger                           (Address, StakePubKeyHash (..))
import           Network.HTTP.Client              (HttpException (..), newManager)
import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           Plutus.Script.Utils.Value        (AssetClass (..), CurrencySymbol, TokenName)
import qualified Plutus.V1.Ledger.Value           as P
import           PlutusAppsExtra.Types.Error      (BlockfrostError (UnknownNetworkMagic), ConnectionError (..))
import           PlutusAppsExtra.Utils.Address    (spkhToStakeCredential)
import           PlutusAppsExtra.Utils.Blockfrost (AccDelegationHistoryResponse (..), AssetHistoryResponse, AssetTxsResponse (..),
                                                   Bf (..), BfOrder (..), TxDelegationsCertsResponse, TxUTxoResponseOutput (..),
                                                   TxUtxoResponse (..), TxUtxoResponseInput (..))
import           Servant.API                      (Capture, Get, Header, JSON, QueryParam, (:<|>) ((:<|>)), (:>))
import           Servant.Client                   (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import qualified Servant.Client                   as Servant

tokenFilePath :: FilePath
tokenFilePath = "blockfrost.token"

portBf :: Int
portBf = 80

getAddressFromStakePubKeyHash :: NetworkId -> PoolId -> StakePubKeyHash -> IO (Maybe Address)
getAddressFromStakePubKeyHash network poolId spkh = runMaybeT $ do
    history <- MaybeT $ sequence $ getAccountDelegationHistory network . makeStakeAddress network <$> spkhToStakeCredential spkh
    txHash  <- MaybeT $ pure $ adhrTxHash <$> find ((== poolId) . adhrPoolId) history
    MaybeT $ fmap turiAddress . listToMaybe . turInputs <$> getTxUtxo network txHash

getStakeAddressLastPool :: NetworkId -> StakeAddress -> IO (Maybe PoolId)
getStakeAddressLastPool network stakeAddr = fmap adhrPoolId . listToMaybe <$> getAccountDelegationHistory network stakeAddr

getAddressFromStakeAddress :: NetworkId -> StakeAddress -> IO (Maybe Address)
getAddressFromStakeAddress network stakeAddr = do
    txId <- fmap adhrTxHash . listToMaybe <$> getAccountDelegationHistory network stakeAddr
    maybe (pure Nothing) (fmap (fmap turiAddress . listToMaybe . turInputs) . getTxUtxo network) txId

-- find tx id where address have minted specific amount of asset
verifyAsset :: NetworkId -> CurrencySymbol -> TokenName -> Integer -> Address -> IO (Maybe TxId)
verifyAsset network cs token amount addr = do
    history <- getAssetTxs network cs token
    foldM (\res (atrTxHash -> txId) -> (res <|>) <$> (getTxUtxo network txId <&> findOutput txId . turOutputs)) Nothing history
    where
        findOutput txId outs = const (Just txId) =<< find (\o -> turoAddress o == addr && P.valueOf (turoAmount o) cs token == amount) outs

verifyAssetFast 
    :: NetworkId
    -> CurrencySymbol
    -> TokenName
    -> [(Address, Integer)]
    -> Maybe ([(Address, Integer, TxId)] -> IO ()) -- Function to save intermidiate results
    -> [(Address, Integer, Cardano.Api.TxId)]      -- Already verified addresses
    -> IO [Either Address (Address, Integer, Cardano.Api.TxId)]
verifyAssetFast network cs token recepients saveIntermidiate verified = do
        history <- getAssetTxs network cs token
        go recepients $ filter ((`notElem` map (^. _3) verified) . atrTxHash) history
    where
        total = length recepients
        -- end of recepients list
        go [] _ = pure []
        -- end of token history
        go rs [] = pure $ map (Left . fst) rs
        go rs ((atrTxHash -> txId) : hs) = do
            pairs <- map (\o -> (turoAddress o, P.valueOf (turoAmount o) cs token)) . turOutputs <$> getTxUtxo network txId
            let res = map (\(a,b) -> (a,b,txId)) $ pairs `intersect` rs
                currentCounter = total - length rs
            liftIO $ maybe (pure ()) ($ res) saveIntermidiate
            liftIO $ putStrLn $ show currentCounter <> "/" <> show total
            (map Right res <>) <$> go (rs \\ pairs) hs

--------------------------------------------------- Blockfrost API ---------------------------------------------------

getTxUtxo :: NetworkId -> TxId -> IO TxUtxoResponse
getTxUtxo network txId = getFromEndpointBF network $ withBfToken $ \t -> getBfTxUtxo t $ Bf txId

getAccountDelegationHistory :: NetworkId -> StakeAddress -> IO [AccDelegationHistoryResponse]
getAccountDelegationHistory network addr = getFromEndpointBF network $ withBfToken $ \t -> 
        getBfAccDelegationHistory t (Bf addr) (Just Desc)

getAssetTxs :: NetworkId -> CurrencySymbol -> TokenName -> IO [AssetTxsResponse]
getAssetTxs network cs name = go 1
    where go n = do
            res <- getFromEndpointBF network $ withBfToken $ \t -> getBfAssetTxs t (Bf $ AssetClass (cs, name)) (Just n)
            case res of
                [] -> pure []
                xs -> (xs <>) <$> go (n + 1)

getAssetHistory :: NetworkId -> CurrencySymbol -> TokenName -> IO [AssetHistoryResponse]
getAssetHistory network cs name = getFromEndpointBF network $ withBfToken $ \t -> getBfAssetHistory t (Bf $ AssetClass (cs, name))

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