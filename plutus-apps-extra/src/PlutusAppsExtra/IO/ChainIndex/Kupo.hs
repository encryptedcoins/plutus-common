{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module PlutusAppsExtra.IO.ChainIndex.Kupo where

import           Cardano.Api                      (NetworkId, writeFileJSON)
import           Control.Applicative              (Applicative (..))
import           Control.Concurrent               (threadDelay)
import           Control.Exception                (AsyncException (UserInterrupt), Exception (..), SomeException)
import           Control.FromSum                  (eitherToMaybe)
import           Control.Monad                    (forM, join, (>=>))
import           Control.Monad.Catch              (MonadCatch, MonadThrow (throwM), handle, try)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Data.Aeson                       (FromJSON (parseJSON), ToJSON, eitherDecodeFileStrict)
import           Data.Aeson.Types                 (parseMaybe)
import           Data.Data                        (Proxy (..))
import           Data.Default                     (Default (def))
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Time                        (getCurrentTime)
import           GHC.Base                         (coerce)
import           GHC.TypeLits                     (AppendSymbol, KnownSymbol)
import           Ledger                           (Address (..), Datum, DatumFromQuery (..), DatumHash, DecoratedTxOut (..),
                                                   Script, ScriptHash, Slot (getSlot), TxId, TxOutRef (..), Validator,
                                                   ValidatorHash, Versioned, fromCardanoValue, stakingCredential)
import           Network.HTTP.Client              (HttpExceptionContent, Request)
import           Plutus.V2.Ledger.Api             (Credential (..), CurrencySymbol, TokenName)
import qualified Plutus.V2.Ledger.Api             as P
import           PlutusAppsExtra.Types.Error      (ConnectionError)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import           PlutusAppsExtra.Utils.Kupo       (Kupo (..), KupoOrder, KupoResponse (..), MkPattern (..), Pattern (..),
                                                   fromKupoDatumType, kupoResponseToJSON, GetHealthResponse)
import           PlutusAppsExtra.Utils.Servant    (Endpoint, getFromEndpointOnPort, pattern ConnectionErrorOnPort)
import qualified PlutusTx.AssocMap                as PAM
import           Servant.API                      (Capture, Get, JSON, QueryFlag, QueryParam, (:>))
import           Servant.Client                   (client)

----------------------------------------------------------- Hi-level API -----------------------------------------------------------

-- Get all unspent utxos at a given address
getUtxosAt :: Address -> IO MapUTXO
getUtxosAt addr = getResponse >>= responseToUtxoMap
    where
        getResponse = getKupoResponse @'SUUnspent @'CSCreated @'CSCreated
            def {reqPattern = mkPattern addr, reqSpentOrUnspent = True}

getUnspentTxOutFromRef :: TxOutRef -> IO (Maybe DecoratedTxOut)
getUnspentTxOutFromRef = (getResponse . mkPattern) >=> fmap (join . listToMaybe) . mapM kupoResponseToDecoratedTxOut
    where
        getResponse pat = getKupoResponse @'SUUnspent @'CSCreated @'CSCreated
            def {reqPattern = pat, reqSpentOrUnspent = True}

getTxOutFromRef :: TxOutRef -> IO (Maybe DecoratedTxOut)
getTxOutFromRef ref = getResponse >>= fmap (join . listToMaybe) . mapM kupoResponseToDecoratedTxOut
    where
        getResponse = getKupoResponse @'SUUnspent @'CSCreated @'CSCreated
            def {reqPattern = mkPattern ref}

-- Don't use this function to get total token distribution - response will be to large to handle.
getTokenBalanceToSlot :: MkPattern addrPatt => CurrencySymbol -> TokenName -> Maybe Slot -> addrPatt -> IO Integer
getTokenBalanceToSlot cs tokenName slotTo addrPat = sum . fmap getAmount <$> liftA2 (<>) getSpent getUnspent
    where
        getAmount KupoResponse{..} = fromMaybe 0 $ PAM.lookup cs (P.getValue $ fromCardanoValue krValue) >>= PAM.lookup tokenName
        getUnspent = getKupoResponse @'SUUnspent @'CSCreated @'CSCreated req
        getSpent   = case slotTo of
            Just s -> getKupoResponse @'SUSpent   @'CSCreated @'CSSpent   req{reqCreatedOrSpentAfter = Just s}
            _      -> pure []
        req = def
            { reqPattern              = mkPattern addrPat
            , reqSpentOrUnspent       = True
            , reqCreatedOrSpentBefore = slotTo
            , reqCurrencySymbol       = Just cs
            , reqTokenName            = Just tokenName
            }

----------------------------------------------------------- Get Matches (*) -----------------------------------------------------------

type GetKupoResponse spentOrUnspent createdOrSpentBefore createdOrSpentAfter
    =             "matches"
    :> Capture    "pattern"            Pattern
    :> QueryFlag  spentOrUnspent
    :> QueryParam "order"              KupoOrder
    :> QueryParam createdOrSpentBefore (Kupo Slot)
    :> QueryParam createdOrSpentAfter  (Kupo Slot)
    :> QueryParam "policy_id"          (Kupo CurrencySymbol)
    :> QueryParam "asset_name"         (Kupo TokenName)
    :> QueryParam "transaction_id"     (Kupo TxId)
    :> QueryParam "output_index"       Integer
    :> Get '[JSON] [KupoResponse]

data SpentOrUnspent = SUSpent | SUUnspent
type family SpentOrUnspentSymbol su where
    SpentOrUnspentSymbol 'SUSpent   = "spent"
    SpentOrUnspentSymbol 'SUUnspent = "unspent"

data CreatedOrSpent = CSCreated | CSSpent
type family CreatedOrSpentSymbol cs where
    CreatedOrSpentSymbol 'CSCreated = "created"
    CreatedOrSpentSymbol 'CSSpent   = "spent"

data KupoRequest (su :: SpentOrUnspent) (before :: CreatedOrSpent) (after :: CreatedOrSpent)
    = KupoRequest
    { reqPattern              :: Pattern
    , reqSpentOrUnspent       :: Bool
    , reqOrder                :: Maybe KupoOrder
    , reqCreatedOrSpentBefore :: Maybe Slot
    , reqCreatedOrSpentAfter  :: Maybe Slot
    , reqCurrencySymbol       :: Maybe CurrencySymbol
    , reqTokenName            :: Maybe TokenName
    , reqTxId                 :: Maybe TxId
    , reqTxIdx                :: Maybe Integer
    } deriving (Show, Eq)

instance Default (KupoRequest su before after) where
    def = KupoRequest WildCardPattern False Nothing Nothing Nothing Nothing Nothing Nothing Nothing

type IsValidRequest su before after =
    ( KnownSymbol (SpentOrUnspentSymbol su)
    , KnownSymbol (AppendSymbol (CreatedOrSpentSymbol before) "_before")
    , KnownSymbol (AppendSymbol (CreatedOrSpentSymbol after) "_after")
    )

getKupoResponse :: forall su before after. IsValidRequest su before after => KupoRequest su before after -> IO [KupoResponse]
getKupoResponse KupoRequest{..} = getFromEndpointKupo $ client p
        reqPattern
        reqSpentOrUnspent
        reqOrder
        (Kupo <$> reqCreatedOrSpentBefore)
        (Kupo <$> reqCreatedOrSpentAfter)
        (Kupo <$> reqCurrencySymbol)
        (Kupo <$> reqTokenName)
        (Kupo <$> reqTxId)
        reqTxIdx
    where
        p = Proxy @(GetKupoResponse
            (SpentOrUnspentSymbol su)
            (AppendSymbol (CreatedOrSpentSymbol before) "_before")
            (AppendSymbol (CreatedOrSpentSymbol after)  "_after"))

------------------------------------------------------------- Get By hash -------------------------------------------------------------

type GetScriptByHash
    = "scripts" :> Capture "script hash" (Kupo ScriptHash) :> Get '[JSON] (Maybe (Kupo (Versioned Script)))

getScriptByHash :: ScriptHash -> IO (Maybe (Versioned Script))
getScriptByHash = fmap coerce . getFromEndpointKupo . client (Proxy @GetScriptByHash) . Kupo

type GetValidatorByHash
    = "scripts" :> Capture "validator hash" (Kupo ValidatorHash) :> Get '[JSON] (Maybe (Kupo (Versioned Validator)))

getValidatorByHash :: ValidatorHash -> IO (Maybe (Versioned Validator))
getValidatorByHash = fmap coerce . getFromEndpointKupo . client (Proxy @GetValidatorByHash) . Kupo

type GetDatumByHash
    = "datums"  :> Capture "datum hash" (Kupo DatumHash) :> Get '[JSON] (Kupo Datum)

getDatumByHash :: DatumHash -> IO (Maybe Datum)
getDatumByHash = fmap eitherToMaybe . try @IO @SomeException . fmap coerce . getFromEndpointKupo . client (Proxy @GetDatumByHash) . Kupo

------------------------------------------------------------ Partialy getting ------------------------------------------------------------

-- Partially get large kupo responses, with intermediate saving of the result.
partiallyGet :: forall su b a m. (MonadIO m, MonadCatch m, IsValidRequest su b a)
    => NetworkId -> (Text -> m ()) -> Slot -> Slot -> Slot -> KupoRequest su b a -> FilePath -> m [KupoResponse]
partiallyGet newtworkId ((. T.pack) -> mkLog) slotFrom slotTo slotDelta req fp = do
    let intervals = divideTimeIntoIntervals slotFrom slotTo slotDelta
    resValue <- fmap concat $ forM (zip [1 :: Int ..] intervals) $ \(i, (f, t)) -> reloadHandler $ do
        mkLog $ show i <> "/" <> show (length intervals)
        let fileName = fp <> show (getSlot f) <> "_"  <> show (getSlot t) <> ".json"
            getResponse = liftIO $ fmap (kupoResponseToJSON newtworkId) <$> do
                let req' = req{reqCreatedOrSpentAfter = Just f, reqCreatedOrSpentBefore = Just t} :: KupoRequest su b a
                getKupoResponse req'
        withResultSaving fileName getResponse
    pure $ catMaybes $ parseMaybe parseJSON <$> resValue
    where
        reloadHandler ma = (`handle` ma) $ \e -> case fromException e of
            Just UserInterrupt -> throwM UserInterrupt
            _ -> do
                ct <- liftIO getCurrentTime
                mkLog ( show ct <> "\n" <> show e <> "\n(Handled)")
                liftIO (threadDelay 5_000_000)
                reloadHandler ma

divideTimeIntoIntervals :: Slot -> Slot -> Slot -> [(Slot, Slot)]
divideTimeIntoIntervals from to delta = do
    let xs = [from, from + delta .. to]
    zip (init xs) (subtract 1 <$> tail xs) <> [(last xs, to)]

withResultSaving :: (MonadIO m, FromJSON a, ToJSON a) => FilePath -> m a -> m a
withResultSaving fp action = liftIO (try @_ @SomeException $ either error id <$> eitherDecodeFileStrict fp) >>= either doAction pure
    where
        doAction _ = do
            res <- action
            _ <- liftIO $ writeFileJSON fp res
            pure res

-------------------------------------------------------- Kupo response conversion --------------------------------------------------------

responseToUtxoMap :: [KupoResponse] -> IO MapUTXO
responseToUtxoMap responses = Map.fromList . mapMaybe sequence <$> mapM (\r -> (kupoResponseToTxOutRef r,) <$> kupoResponseToDecoratedTxOut r) responses

kupoResponseToDecoratedTxOut :: KupoResponse -> IO (Maybe DecoratedTxOut)
kupoResponseToDecoratedTxOut KupoResponse{..} = do

    mbDat <- fmap join $ sequence $ getDatumByHash <$> krDatumHash
    _decoratedTxOutReferenceScript <- fmap join $ sequence $ getScriptByHash <$> krScriptHash
    let _decoratedTxOutValue = krValue
    case addressCredential krAddress of

        PubKeyCredential _decoratedTxOutPubKeyHash -> do
            let _decoratedTxOutStakingCredential = addressStakingCredential krAddress
                _decoratedTxOutPubKeyDatum = liftA2 (,) krDatumHash (liftA2 ($) (fromKupoDatumType <$> krDatumType) mbDat)
            pure $ Just PublicKeyDecoratedTxOut{..}

        ScriptCredential _decoratedTxOutValidatorHash -> do
            let _decoratedTxOutStakingCredential = stakingCredential krAddress
                dfq = fromMaybe DatumUnknown $ liftA2 ($) (fromKupoDatumType <$> krDatumType) mbDat
            _decoratedTxOutValidator <- getValidatorByHash _decoratedTxOutValidatorHash
            case krDatumHash of
                Nothing -> pure Nothing
                Just dh -> pure $ Just ScriptDecoratedTxOut{_decoratedTxOutScriptDatum = (dh, dfq), ..}

kupoResponseToTxOutRef :: KupoResponse -> TxOutRef
kupoResponseToTxOutRef KupoResponse{..} = TxOutRef krTxId krOutputIndex

------------------------------------------------------------- Kupo port -------------------------------------------------------------

getFromEndpointKupo :: Endpoint a
getFromEndpointKupo = getFromEndpointOnPort 1442

pattern KupoConnectionError :: Request -> HttpExceptionContent -> ConnectionError
pattern KupoConnectionError req content <- ConnectionErrorOnPort 1442 req content

type GetHealth = "health" :> Get '[JSON] GetHealthResponse

getHealth :: IO GetHealthResponse
getHealth = getFromEndpointKupo $ client (Proxy @GetHealth)