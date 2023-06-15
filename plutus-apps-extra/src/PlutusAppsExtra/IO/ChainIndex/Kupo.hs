{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module PlutusAppsExtra.IO.ChainIndex.Kupo where

import           Control.Exception                (SomeException, try)
import           Control.FromSum                  (eitherToMaybe, maybeToEither)
import           Control.Monad                    (join, (<=<))
import           Data.Coerce                      (coerce)
import           Data.Data                        (Proxy (..))
import           Data.Function                    (on)
import qualified Data.List                        as L
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, listToMaybe, fromMaybe)
import           Ledger                           (Address (..), Datum (..), DatumHash (..), DecoratedTxOut (..), Script,
                                                   ScriptHash (..), Slot, TokenName, TxOutRef (..), Validator (..),
                                                   ValidatorHash (..), Versioned (..), AssetClass, PubKeyHash, CurrencySymbol)
import           Ledger.Value                     (Value (..), assetClassValueOf)
import           Network.HTTP.Client              (HttpExceptionContent, Request)
import           Plutus.V1.Ledger.Api             (StakingCredential(..), Credential (PubKeyCredential))
import           PlutusAppsExtra.Types.Error      (ConnectionError)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import           PlutusAppsExtra.Utils.Kupo       (Kupo (..), KupoDecoratedTxOut (..), KupoUTXO, KupoUTXOs, KupoAddress, anyAddress,
                                                   mkAddressWithAnyCred, mkKupoAddress, Pattern (..), KupoResponse (..), KupoWildCard (..))
import qualified PlutusAppsExtra.Utils.Kupo       as Kupo
import           PlutusAppsExtra.Utils.Servant    (Endpoint, getFromEndpointOnPort, pattern ConnectionErrorOnPort)
import qualified PlutusTx.AssocMap                as PAM
import           Servant.API                      (Capture, Get, JSON, QueryFlag, QueryParam, (:<|>) ((:<|>)), (:>))
import           Servant.Client                   (ClientM, client)

-- Get all utxos at a given address
getUtxosAt :: Address -> IO MapUTXO
getUtxosAt = fromKupoUtxos <=< (getFromEndpointKupo . getKupoUtxosAt . mkKupoAddress)

getUnspentTxOutFromRef :: TxOutRef -> IO (Maybe DecoratedTxOut)
getUnspentTxOutFromRef = sequence . listToMaybe . fmap fromKupoDecoratedTxOut <=<
    (getFromEndpointKupo . getKupoTxOutFromRef True . Kupo)

getTxOutFromRef :: TxOutRef -> IO (Maybe DecoratedTxOut)
getTxOutFromRef = sequence . listToMaybe . fmap fromKupoDecoratedTxOut <=<
    (getFromEndpointKupo . getKupoTxOutFromRef False . Kupo)

getSciptByHash :: ScriptHash -> IO (Maybe (Versioned Script))
getSciptByHash = fmap coerce . getFromEndpointKupo . getKupoScriptByHash . Kupo

getValidatorByHash :: ValidatorHash -> IO (Maybe (Versioned Validator))
getValidatorByHash = fmap coerce . getFromEndpointKupo . getKupoValidatorByHash . Kupo

getDatumByHash :: DatumHash -> IO Datum
getDatumByHash = fmap coerce . getFromEndpointKupo . getKupoDatumByHash . Kupo

getDatumByHashSafe :: DatumHash -> IO (Maybe Datum)
getDatumByHashSafe = fmap eitherToMaybe . try @SomeException . getDatumByHash

-- Get all specified utxos and filter them
getKupoUtxosWith :: IO KupoUTXOs -> ((TxOutRef, KupoDecoratedTxOut) -> Bool) -> IO MapUTXO
getKupoUtxosWith getUtxos f = getUtxos >>= fromKupoUtxos . coerce . filter f . coerce

-- Get all utxos between specified slots, that contain certain numbers of tokens with specified name.
getUtxosWithTokenAmountBetweenSlots :: TokenName -> Integer -> Slot -> Slot -> IO MapUTXO
getUtxosWithTokenAmountBetweenSlots name amt sFrom sTo = getKupoUtxosWith getKupoUtxos f
    where
        f = elem (PAM.singleton name amt) . PAM.elems . getValue . Kupo._decoratedTxOutValue . snd
        getKupoUtxos = getFromEndpointKupo $ getAllKupoUtxosBetweenSlots (Just (Kupo sFrom)) (Just (Kupo sTo)) False

-- Get all unspent utxos between specified slots, that contain specified assetClass
getUnspentUtxosWithAssetBetweenSlots :: AssetClass -> Maybe Slot -> Maybe Slot -> IO MapUTXO
getUnspentUtxosWithAssetBetweenSlots asset createdAfter createdBefore = getKupoUtxosWith getUnspentKupoUtxos f
    where
        f (_, out) = assetClassValueOf (Kupo._decoratedTxOutValue out) asset > 0
        getUnspentKupoUtxos = getFromEndpointKupo $ getAllKupoUtxosBetweenSlots (Kupo <$> createdAfter) (Kupo <$> createdBefore) True

getSpentUtxosWithAssetBetweenSlots :: AssetClass -> Maybe Slot -> Maybe Slot -> IO MapUTXO
getSpentUtxosWithAssetBetweenSlots asset createdBefore spentAfter = getKupoUtxosWith getUnspentKupoUtxos f
    where
        f (_, out) = assetClassValueOf (Kupo._decoratedTxOutValue out) asset > 0
        getUnspentKupoUtxos = getFromEndpointKupo $ getAllKupoUtxosBetweenSlots (Kupo <$> createdBefore) (Kupo <$> spentAfter) True

getKupoResponseByStakeKeyBetweenSlots :: Maybe Slot -> Maybe Slot -> PubKeyHash -> IO [KupoResponse]
getKupoResponseByStakeKeyBetweenSlots createdAfter createdBefore pkh = getFromEndpointKupo $ getKupoResponseBetweenSlotsCC
    (Kupo <$> createdAfter)
    (Kupo <$> createdBefore)
    False
    (AddrPattern $ mkAddressWithAnyCred $ StakingHash $ PubKeyCredential pkh)

getKupoResponseByAssetClassBetweenSlotsCC :: Maybe Slot -> Maybe Slot -> CurrencySymbol -> Maybe TokenName -> IO [KupoResponse]
getKupoResponseByAssetClassBetweenSlotsCC createdAfter createdBefore cs tokenName
    = getFromEndpointKupo $ getKupoResponseBetweenSlotsCC
        (Kupo <$> createdAfter)
        (Kupo <$> createdBefore)
        True
        (AssetPattern (cs, maybeToEither KupoWildCard tokenName))

getKupoResponseByAssetClassBetweenSlotsCS :: Maybe Slot -> Maybe Slot -> CurrencySymbol -> Maybe TokenName -> IO [KupoResponse]
getKupoResponseByAssetClassBetweenSlotsCS createdBefore spentAfter cs tokenName
    = getFromEndpointKupo $ getKupoResponseBetweenSlotsCS
        (Kupo <$> createdBefore)
        (Kupo <$> spentAfter)
        True
        (AssetPattern (cs, maybeToEither KupoWildCard tokenName))

getKupoResponseBetweenSlots :: Maybe Slot -> Maybe Slot -> IO [KupoResponse]
getKupoResponseBetweenSlots createdAfter createdBefore 
    = getFromEndpointKupo $ getKupoResponseBetweenSlotsCC
        (Kupo <$> createdAfter)
        (Kupo <$> createdBefore)
        False
        WildCardPattern

getAssetDistributionBeforeSlot :: CurrencySymbol -> Maybe TokenName -> Maybe Slot -> IO (Map.Map Address Integer)
getAssetDistributionBeforeSlot cs mbTokenName slot = do
        unspentResponses <- getKupoResponseByAssetClassBetweenSlotsCC Nothing slot cs mbTokenName
        spentResponses   <- getKupoResponseByAssetClassBetweenSlotsCS slot slot cs mbTokenName
        let distribution = (\KupoResponse{..} -> (krAddress, assetAmount krValue)) <$> unspentResponses <> spentResponses
            distribution' = fmap (\xs -> (fst $ head xs, sum $ map snd xs)) $ L.group $ L.sortBy (compare `on` fst) distribution
        pure $  Map.fromList distribution'
    where
        assetAmount (Value val) = do
            let csMap = fromMaybe PAM.empty $ PAM.lookup cs val
            case mbTokenName of
                Nothing -> sum $ PAM.elems csMap
                Just tokenName -> fromMaybe 0 $ PAM.lookup tokenName csMap

--------------------------------------------------- Kupo API ---------------------------------------------------

type KupoAPI
    =    GetUtxosAt
    :<|> UnspetTxOutFromRef
    :<|> GetUtxosBetweenSlots
    :<|> GetSpentUtxosBetweenSlots
    :<|> GetKupoResponseBetweenSlots
    :<|> GetSpentKupoResponseBetweenSlots
    :<|> GetScriptByHash
    :<|> GetValidatorByHash
    :<|> GetDatumByHash

getFromEndpointKupo :: Endpoint a
getFromEndpointKupo = getFromEndpointOnPort 1442

pattern KupoConnectionError :: Request -> HttpExceptionContent -> ConnectionError
pattern KupoConnectionError req content <- ConnectionErrorOnPort _ req content

type GetUtxosAt         =
    "matches" :> Capture "pattern" KupoAddress :> QueryFlag "unspent" :> Get '[JSON] [Maybe KupoUTXO]
type UnspetTxOutFromRef =
    "matches" :> Capture "pattern" (Kupo TxOutRef) :> QueryFlag "unspent" :> Get '[JSON] [Maybe KupoDecoratedTxOut]
type GetUtxosBetweenSlots =
    "matches" :> Capture "pattern" Pattern
              :> QueryParam "created_after"  (Kupo Slot)
              :> QueryParam "created_before" (Kupo Slot)
              :> QueryFlag "unspent"
              :> Get '[JSON] [Maybe KupoUTXO]
type GetSpentUtxosBetweenSlots =
    "matches" :> Capture "pattern" Pattern
              :> QueryParam "created_before"  (Kupo Slot)
              :> QueryParam "spent_after" (Kupo Slot)
              :> QueryFlag "spent"
              :> Get '[JSON] [Maybe KupoUTXO]
type GetKupoResponseBetweenSlots =
    "matches" :> Capture "pattern" Pattern
              :> QueryParam "created_after"  (Kupo Slot)
              :> QueryParam "created_before" (Kupo Slot)
              :> QueryFlag "unspent"
              :> Get '[JSON] [KupoResponse]
type GetSpentKupoResponseBetweenSlots =
    "matches" :> Capture "pattern" Pattern
              :> QueryParam "created_before" (Kupo Slot)
              :> QueryParam "spent_after"    (Kupo Slot)
              :> QueryFlag "spent"
              :> Get '[JSON] [KupoResponse]
type GetScriptByHash    =
    "scripts" :> Capture "script hash" (Kupo ScriptHash) :> Get '[JSON] (Maybe (Kupo (Versioned Script)))
type GetValidatorByHash =
    "scripts" :> Capture "validator hash" (Kupo ValidatorHash) :> Get '[JSON] (Maybe (Kupo (Versioned Validator)))
type GetDatumByHash     =
    "datums"  :> Capture "datum hash" (Kupo DatumHash) :> Get '[JSON] (Kupo Datum)

getKupoUtxosAt              :: KupoAddress        -> ClientM KupoUTXOs
getKupoTxOutFromRef             :: Bool -> Kupo TxOutRef      -> ClientM [KupoDecoratedTxOut]
getAllKupoUtxosBetweenSlots :: Maybe (Kupo Slot)  -> Maybe (Kupo Slot) -> Bool -> ClientM KupoUTXOs
getSpentKupoUtxosBetweenSlots ::  Maybe (Kupo Slot)  -> Maybe (Kupo Slot) -> Bool -> ClientM KupoUTXOs
getKupoResponseBetweenSlotsCC :: Maybe (Kupo Slot) -> Maybe (Kupo Slot) -> Bool -> Pattern -> ClientM [KupoResponse]
getKupoResponseBetweenSlotsCS :: Maybe (Kupo Slot) -> Maybe (Kupo Slot) -> Bool -> Pattern -> ClientM [KupoResponse]
getKupoScriptByHash         :: Kupo ScriptHash    -> ClientM (Maybe (Kupo (Versioned Script)))
getKupoValidatorByHash      :: Kupo ValidatorHash -> ClientM (Maybe (Kupo (Versioned Validator)))
getKupoDatumByHash          :: Kupo DatumHash     -> ClientM (Kupo Datum)
(getKupoUtxosAt
    , getKupoTxOutFromRef
    , getAllKupoUtxosBetweenSlots
    , getSpentKupoUtxosBetweenSlots
    , getKupoResponseBetweenSlotsCC
    , getKupoResponseBetweenSlotsCS
    , getKupoScriptByHash
    , getKupoValidatorByHash
    , getKupoDatumByHash
    )
    = (fmap catMaybes <$> (`getKupoUtxosAt_` True)
      ,\isUnspent -> fmap catMaybes <$> (`getKupoUnspentTxOutFromRef_` isUnspent)
      ,\f t isUnspent-> catMaybes <$> getKupoUtxosBetweenSlots_ (AddrPattern anyAddress) f t isUnspent
      ,\f t isSpent-> catMaybes <$> getKupoSpentUtxosBetweenSlots_ (AddrPattern anyAddress) f t isSpent
      ,\f t isUnspent pat -> getKupoResponseBetweenSlotsCC_ pat f t isUnspent
      ,\f t isSpent pat -> getKupoResponseBetweenSlotsCS_ pat f t isSpent
      ,getKupoScriptByHash_
      ,getKupoValidatorByHash_
      ,getKupoDatumByHash_
      )
    where
        getKupoUtxosAt_
            :<|> getKupoUnspentTxOutFromRef_
            :<|> getKupoUtxosBetweenSlots_
            :<|> getKupoSpentUtxosBetweenSlots_
            :<|> getKupoResponseBetweenSlotsCC_
            :<|> getKupoResponseBetweenSlotsCS_
            :<|> getKupoScriptByHash_
            :<|> getKupoValidatorByHash_
            :<|> getKupoDatumByHash_ = client (Proxy @KupoAPI)

fromKupoUtxos :: KupoUTXOs -> IO MapUTXO
fromKupoUtxos = fmap Map.fromList . mapM (mapM fromKupoDecoratedTxOut . coerce)

fromKupoDecoratedTxOut :: KupoDecoratedTxOut -> IO DecoratedTxOut
fromKupoDecoratedTxOut = \case
    KupoPublicKeyDecoratedTxOut{..} -> do
        _decoratedTxOutPubKeyDatum     <- maybe (pure Nothing) (fmap Just . getDatum) _decoratedTxOutPubKeyDatumHash
        _decoratedTxOutReferenceScript <- getScript _decoratedTxOutReferenceScriptHash
        pure PublicKeyDecoratedTxOut{..}
    KupoScriptDecoratedTxOut{..} -> do
        _decoratedTxOutScriptDatum     <- getDatum  _decoratedTxOutScriptDatum
        _decoratedTxOutReferenceScript <- getScript _decoratedTxOutReferenceScriptHash
        _decoratedTxOutValidator       <- getValidatorByHash _decoratedTxOutValidatorHash
        pure ScriptDecoratedTxOut{..}
    where
        getScript = fmap join . mapM getSciptByHash
        getDatum (dh, dtype) = (dh,) . dtype <$> getDatumByHash dh
