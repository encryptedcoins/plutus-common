{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module PlutusAppsExtra.IO.ChainIndex.Kupo where

import           Cardano.Api                      (NetworkId, writeFileJSON)
import           Control.Concurrent               (threadDelay)
import           Control.Exception                (AsyncException (UserInterrupt), Exception (..), SomeException)
import           Control.Lens                     (preview, (.~), (<&>), (^?))
import           Control.Monad                    (forM, join, (>=>))
import           Control.Monad.Catch              (MonadCatch, MonadThrow (throwM), handle, try)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Data.Aeson                       (FromJSON (parseJSON), ToJSON, eitherDecodeFileStrict)
import           Data.Aeson.Types                 (parseMaybe)
import           Data.Coerce                      (coerce)
import           Data.Default                     (Default (def))
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Time                        (getCurrentTime)
import           Ledger                           (Address (..), Datum (..), DatumFromQuery (..), DecoratedTxOut (..), Slot (getSlot),
                                                   TxOutRef (..), ValidatorHash (..), decoratedTxOutDatum, decoratedTxOutReferenceScript,
                                                   decoratedTxOutValidator, decoratedTxOutValidatorHash, fromCardanoValue)
import           PlutusAppsExtra.Api.Kupo         (CreatedOrSpent (..), IsValidRequest, KupoRequest (..), SpentOrUnspent (..),
                                                   getDatumByHash, getKupoResponse, getScriptByHash, getValidatorByHash)
import           PlutusAppsExtra.Types.Tx         (UtxoRequirement (..), UtxoRequirements)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import qualified PlutusAppsExtra.Utils.Datum      as Datum
import           PlutusAppsExtra.Utils.Kupo       (KupoResponse (..), MkPattern (..), fromKupoDatumType, kupoResponseToJSON)
import           PlutusLedgerApi.V3               (Credential (..), CurrencySymbol, ToData (..), TokenName)
import qualified PlutusLedgerApi.V3               as P
import qualified PlutusTx.AssocMap                as PAM

----------------------------------------------------------- Hi-level API -----------------------------------------------------------

-- Get all unspent utxos at a given address
getUtxosAt :: UtxoRequirements -> Address -> IO MapUTXO
getUtxosAt reqs addr = getResponse >>= mkEvaluatedMapUtxo reqs
    where
        getResponse = getKupoResponse @'SUUnspent @'CSCreated @'CSCreated
            def {reqPattern = mkPattern addr, reqSpentOrUnspent = True}

getUnspentTxOutFromRef :: UtxoRequirements -> TxOutRef -> IO (Maybe DecoratedTxOut)
getUnspentTxOutFromRef reqs = (getResponse . mkPattern) >=> fmap (join . listToMaybe) . mapM (mkEvaluatedDecoratedTxOut reqs)
    where
        getResponse pat = getKupoResponse @'SUUnspent @'CSCreated @'CSCreated
            def {reqPattern = pat, reqSpentOrUnspent = True}

getTxOutFromRef :: UtxoRequirements -> TxOutRef -> IO (Maybe DecoratedTxOut)
getTxOutFromRef reqs ref = getResponse >>= fmap (join . listToMaybe) . mapM (mkEvaluatedDecoratedTxOut reqs)
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
            Just s -> getKupoResponse @'SUSpent   @'CSCreated @'CSSpent req{reqCreatedOrSpentAfter = Just s}
            _      -> pure []
        req = def
            { reqPattern              = mkPattern addrPat
            , reqSpentOrUnspent       = True
            , reqCreatedOrSpentBefore = slotTo
            , reqCurrencySymbol       = Just cs
            , reqTokenName            = Just tokenName
            }

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
    pure $ mapMaybe (parseMaybe parseJSON) resValue
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

mkEvaluatedMapUtxo :: UtxoRequirements -> [KupoResponse] -> IO MapUTXO
mkEvaluatedMapUtxo reqs =
    fmap (Map.fromList . catMaybes) . traverse (\r -> fmap (kupoResponseToTxOutRef r,) <$> mkEvaluatedDecoratedTxOut reqs r)

mkEvaluatedDecoratedTxOut :: UtxoRequirements -> KupoResponse -> IO (Maybe DecoratedTxOut)
mkEvaluatedDecoratedTxOut reqs KupoResponse{..} = sequence $ mkUnevaluatedDecoratedTxOut KupoResponse{..} <&> \txOut -> do
            fd <- evaluateWhen RequiresDatum     evaluateDatum     $ liftA2 (,) (fst <$> txOut ^? decoratedTxOutDatum) krDatumType
            fs <- evaluateWhen RequiresScript    evaluateScript      krScriptHash
            fv <- evaluateWhen RequiresValidator evaluateValidator $ preview decoratedTxOutValidatorHash txOut
            pure $ fd . fs . fv $ txOut
    where
        evaluateWhen :: UtxoRequirement -> (v -> IO (DecoratedTxOut -> DecoratedTxOut)) -> Maybe v -> IO (DecoratedTxOut -> DecoratedTxOut)
        evaluateWhen req f mbV = case (req `Set.member` reqs, mbV) of
            (True, Just v) -> f v
            _              -> pure id

        evaluateDatum (dh, datType) = do
            dat <- if dh == Datum.unitHash
                   then pure $ Just $ Datum $ toBuiltinData ()
                   else getDatumByHash dh
            let dfq = maybe DatumUnknown (fromKupoDatumType datType) dat
            pure (decoratedTxOutDatum .~ (dh, dfq))

        evaluateScript sh = do
            script <- getScriptByHash sh
            pure (decoratedTxOutReferenceScript .~ script)

        evaluateValidator vh = do
            validator <- getValidatorByHash vh
            pure (decoratedTxOutValidator .~ validator)

mkUnevaluatedDecoratedTxOut :: KupoResponse -> Maybe DecoratedTxOut
mkUnevaluatedDecoratedTxOut KupoResponse{..} =
    let _decoratedTxOutStakingCredential = addressStakingCredential krAddress
        _decoratedTxOutValue             = krValue
        _decoratedTxOutPubKeyDatum       = dat
        _decoratedTxOutReferenceScript   = Nothing
        _decoratedTxOutValidator         = Nothing
    in case addressCredential krAddress of
        PubKeyCredential _decoratedTxOutPubKeyHash    -> pure PublicKeyDecoratedTxOut{..}
        ScriptCredential (coerce -> _decoratedTxOutValidatorHash) -> do
            _decoratedTxOutScriptDatum <- _decoratedTxOutPubKeyDatum
            pure ScriptDecoratedTxOut{..}
    where
        dat = if krDatumHash == Just Datum.unitHash
              then fmap (Datum.unitHash,) $ fromKupoDatumType <$> krDatumType <*> Just (Datum $ toBuiltinData ())
              else (, DatumUnknown) <$> krDatumHash

kupoResponseToTxOutRef :: KupoResponse -> TxOutRef
kupoResponseToTxOutRef KupoResponse{..} = TxOutRef krTxId krOutputIndex
