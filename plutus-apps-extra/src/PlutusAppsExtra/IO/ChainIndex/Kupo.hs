{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module PlutusAppsExtra.IO.ChainIndex.Kupo where

import           Control.Monad                    (join, (<=<))
import           Data.Coerce                      (coerce)
import           Data.Data                        (Proxy (..))
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, listToMaybe)
import           Ledger                           (Address (..), Datum (..), DatumHash (..), DecoratedTxOut (..), Script,
                                                   ScriptHash (..), Slot, TokenName, TxOutRef (..), Validator (..),
                                                   ValidatorHash (..), Versioned (..))
import           Ledger.Value                     (Value (getValue))
import           Network.HTTP.Client              (HttpExceptionContent, Request)
import           PlutusAppsExtra.Types.Error      (ConnectionError)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import           PlutusAppsExtra.Utils.Kupo       (Kupo (..), KupoDecoratedTxOut (..), KupoUTXO, KupoUTXOs, KupoWildCard (..))
import qualified PlutusAppsExtra.Utils.Kupo       as Kupo
import           PlutusAppsExtra.Utils.Servant    (Endpoint, getFromEndpointOnPort, pattern ConnectionErrorOnPort)
import qualified PlutusTx.AssocMap                as PAM
import           Servant.API                      (Capture, Get, JSON, QueryFlag, QueryParam, (:<|>) ((:<|>)), (:>))
import           Servant.Client                   (ClientM, client)

-- Get all utxos at a given address
getUtxosAt :: Address -> IO MapUTXO
getUtxosAt = fromKupoUtxos <=< (getFromEndpointKupo . getKupoUtxosAt . Kupo)

getUnspentTxOutFromRef :: TxOutRef -> IO (Maybe DecoratedTxOut)
getUnspentTxOutFromRef = sequence . listToMaybe . fmap fromKupoDecoratedTxOut <=<
    (getFromEndpointKupo . getKupoUnspentTxOutFromRef . Kupo)

getSciptByHash :: ScriptHash -> IO (Maybe (Versioned Script))
getSciptByHash = fmap coerce . getFromEndpointKupo . getKupoScriptByHash . Kupo

getValidatorByHash :: ValidatorHash -> IO (Maybe (Versioned Validator))
getValidatorByHash = fmap coerce . getFromEndpointKupo . getKupoValidatorByHash . Kupo

getDatumByHash :: DatumHash -> IO Datum
getDatumByHash = fmap coerce . getFromEndpointKupo . getKupoDatumByHash . Kupo

-- Get all utxos between specified slots, that contains certain numbers of tokens with specified name.
getUtxosWithTokensBetweenSlots :: TokenName -> Integer -> Slot -> Slot -> IO MapUTXO
getUtxosWithTokensBetweenSlots name amt sFrom sTo = getKupoUtxos >>= fromKupoUtxos . coerce . amtFilter . coerce
    where
        amtFilter :: [(TxOutRef, KupoDecoratedTxOut)] -> [(TxOutRef, KupoDecoratedTxOut)]
        amtFilter = filter (elem (PAM.singleton name amt) . PAM.elems . getValue . Kupo._decoratedTxOutValue . snd)
        getKupoUtxos = getFromEndpointKupo $ getAllKupoUtxosBetweenSlots (Just (Kupo sFrom)) (Just (Kupo sTo))

--------------------------------------------------- Kupo API ---------------------------------------------------

type KupoAPI = GetUtxosAt :<|> UnspetTxOutFromRef :<|> GetAllUtxosBetweenSlots :<|> GetScriptByHash :<|> GetValidatorByHash :<|> GetDatumByHash

getFromEndpointKupo :: Endpoint a
getFromEndpointKupo = getFromEndpointOnPort 1442

pattern KupoConnectionError :: Request -> HttpExceptionContent -> ConnectionError
pattern KupoConnectionError req content <- ConnectionErrorOnPort 1442 req content

type GetUtxosAt         =
    "matches" :> Capture "pattern" (Kupo Address) :> QueryFlag "unspent" :> Get '[JSON] [Maybe KupoUTXO]
type UnspetTxOutFromRef =
    "matches" :> Capture "pattern" (Kupo TxOutRef) :> QueryFlag "unspent" :> Get '[JSON] [Maybe KupoDecoratedTxOut]
type GetAllUtxosBetweenSlots =
    "matches" :> Capture "pattern" KupoWildCard
              :> QueryParam "created_after"  (Kupo Slot)
              :> QueryParam "created_before" (Kupo Slot)
              :> Get '[JSON] [Maybe KupoUTXO]
type GetScriptByHash    =
    "scripts" :> Capture "script hash" (Kupo ScriptHash) :> Get '[JSON] (Maybe (Kupo (Versioned Script)))
type GetValidatorByHash =
    "scripts" :> Capture "validator hash" (Kupo ValidatorHash) :> Get '[JSON] (Maybe (Kupo (Versioned Validator)))
type GetDatumByHash     =
    "datums"  :> Capture "datum hash" (Kupo DatumHash) :> Get '[JSON] (Kupo Datum)

getKupoUtxosAt              :: Kupo Address       -> ClientM KupoUTXOs
getKupoUnspentTxOutFromRef  :: Kupo TxOutRef      -> ClientM [KupoDecoratedTxOut]
getAllKupoUtxosBetweenSlots :: Maybe (Kupo Slot)  -> Maybe (Kupo Slot) -> ClientM KupoUTXOs
getKupoScriptByHash         :: Kupo ScriptHash    -> ClientM (Maybe (Kupo (Versioned Script)))
getKupoValidatorByHash      :: Kupo ValidatorHash -> ClientM (Maybe (Kupo (Versioned Validator)))
getKupoDatumByHash          :: Kupo DatumHash     -> ClientM (Kupo Datum)
(getKupoUtxosAt, getKupoUnspentTxOutFromRef, getAllKupoUtxosBetweenSlots, getKupoScriptByHash, getKupoValidatorByHash, getKupoDatumByHash)
    = (fmap catMaybes <$> (`getKupoUtxosAt_` True)
      ,fmap catMaybes <$> (`getKupoUnspentTxOutFromRef_` True)
      ,\f t -> catMaybes <$> getAllKupoUtxosBetweenSlots_ KupoWildCard f t
      ,getKupoScriptByHash_
      ,getKupoValidatorByHash_
      ,getKupoDatumByHash_
      )
    where
        getKupoUtxosAt_
            :<|> getKupoUnspentTxOutFromRef_
            :<|> getAllKupoUtxosBetweenSlots_
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
