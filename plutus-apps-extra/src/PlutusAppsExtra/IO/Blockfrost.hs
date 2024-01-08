{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <&>" #-}

module PlutusAppsExtra.IO.Blockfrost where

import           Cardano.Api                      (NetworkId (..), StakeAddress, makeStakeAddress)
import           Cardano.Api.Shelley              (PoolId)
import           Control.Applicative              ((<|>))
import           Control.Lens                     (preview, (.~), (^.))
import           Control.Lens.Tuple               (Field3 (_3))
import           Control.Monad                    (foldM)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Data.Either                      (isRight, rights)
import           Data.Foldable                    (find)
import           Data.Functor                     ((<&>))
import           Data.List                        (intersect, (\\))
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set                         as Set
import           Ledger                           (Address (..), Datum (..), DatumFromQuery (..), DatumHash, DecoratedTxOut (..),
                                                   Script, ScriptHash, StakePubKeyHash (..), TxId (..), TxOutRef (..), Validator,
                                                   ValidatorHash, Versioned, decoratedTxOutDatum, decoratedTxOutReferenceScript,
                                                   decoratedTxOutValidator, decoratedTxOutValidatorHash, fromCardanoValue)
import           Plutus.Script.Utils.Value        (CurrencySymbol, TokenName)
import           Plutus.V1.Ledger.Api             (Credential (..), ToData (..))
import qualified Plutus.V1.Ledger.Value           as P
import qualified PlutusAppsExtra.Api.Blockfrost   as Api
import           PlutusAppsExtra.Types.Tx         (UtxoRequirement (..), UtxoRequirements)
import           PlutusAppsExtra.Utils.Address    (bech32ToAddress, spkhToStakeCredential)
import           PlutusAppsExtra.Utils.Blockfrost (AccDelegationHistoryResponse (..), AssetAddressesResponse (..),
                                                   AssetTxsResponse (..), BfOrder (..), TxUtxoResponse (..),
                                                   TxUtxoResponseInput (..), TxUtxoResponseOutput (..), AssetHistoryResponse)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import           PlutusAppsExtra.Utils.Datum      (hashDatum)
import qualified PlutusAppsExtra.Utils.Datum      as Datum
import           PlutusAppsExtra.Utils.Servant    (handle404Maybe)

----------------------------------------------------------- Hi-level API -----------------------------------------------------------

getAddressFromStakePubKeyHash :: NetworkId -> PoolId -> StakePubKeyHash -> IO (Maybe Address)
getAddressFromStakePubKeyHash network poolId spkh = runMaybeT $ do
    history <- MaybeT $ sequence $ Api.getAccountDelegationHistory network . makeStakeAddress network <$> spkhToStakeCredential spkh
    txHash  <- MaybeT $ pure $ adhrTxHash <$> find ((== poolId) . adhrPoolId) history
    MaybeT $ fmap turiAddress . listToMaybe . turInputs <$> Api.getTxUtxo network txHash

getStakeAddressLastPool :: NetworkId -> StakeAddress -> IO (Maybe PoolId)
getStakeAddressLastPool network stakeAddr = fmap adhrPoolId . listToMaybe <$> Api.getAccountDelegationHistory network stakeAddr

getAddressFromStakeAddress :: NetworkId -> StakeAddress -> IO (Maybe Address)
getAddressFromStakeAddress network stakeAddr = do
    txId <- fmap adhrTxHash . listToMaybe <$> Api.getAccountDelegationHistory network stakeAddr
    maybe (pure Nothing) (fmap (fmap turiAddress . listToMaybe . turInputs) . Api.getTxUtxo network) txId

getAccountAssociatedAddresses :: NetworkId -> StakePubKeyHash -> IO (Maybe [Address])
getAccountAssociatedAddresses network spkh = case makeStakeAddress network <$> spkhToStakeCredential spkh of
    Just sa -> Just . mapMaybe bech32ToAddress <$> foldPages (Api.getAccountAssociatedAddresses network sa)
    Nothing -> pure Nothing

getAssetAddressess :: NetworkId -> CurrencySymbol -> TokenName -> IO [(Address, Integer)]
getAssetAddressess network cs name = fmap (map $ \AssetAddressesResponse{..} -> (adrAddress, adrQuantity))
    $ foldPages $ Api.getAssetAddresses network cs name Nothing

getAssetHistory :: NetworkId -> CurrencySymbol -> TokenName -> IO [AssetHistoryResponse]
getAssetHistory network cs name = foldPages $ Api.getAssetHistory network cs name Nothing

getAssetTxs :: NetworkId -> CurrencySymbol -> TokenName -> IO [AssetTxsResponse]
getAssetTxs network cs name = foldPages $ Api.getAssetTxs network cs name Nothing

getTxUtxo :: NetworkId -> UtxoRequirements -> TxId -> IO MapUTXO
getTxUtxo network reqs txId = Api.getTxUtxo network txId >>= mkEvaluatedMapUtxo network reqs

getDatumByHash :: NetworkId -> DatumHash -> IO (Maybe Datum)
getDatumByHash network = handle404Maybe . Api.getDatumByHashUnsafe network

getScriptByHash :: NetworkId -> ScriptHash -> IO (Maybe (Versioned Script))
getScriptByHash network = handle404Maybe . Api.getScriptByHashUnsafe network

getValidatorByHash :: NetworkId -> ValidatorHash -> IO (Maybe (Versioned Validator))
getValidatorByHash network = handle404Maybe . Api.getValidatorByHashUnsafe network

-------------------------------------------- Iterative monitoring (delegated txs)  --------------------------------------------

-- Get ids of txs with asset of the specified policy
-- If txId is passed as the last argument - function will stop after ecnountering this txId
-- This function is useful when you need to iteratively monitor txs related to specific asset
getAllAssetTxsAfterTxId :: NetworkId -> CurrencySymbol -> TokenName -> Maybe TxId -> IO [TxId]
getAllAssetTxsAfterTxId network cs name mbTxId = takeAssetTxsWhile network cs name (Just Desc) pure predicate
    where
        predicate txId = if Just txId == mbTxId then Left txId else Right txId

-- Apply function to txId list to get txs while predicate(either) holds
takeAssetTxsWhile ::  NetworkId -> CurrencySymbol -> TokenName -> Maybe BfOrder -> (TxId -> IO a) -> (a -> Either l r) -> IO [r]
takeAssetTxsWhile network cs name mbOrder getTxInfo predicate = rights <$> takeAssetTxsWhileWithResult network cs name mbOrder getTxInfo predicate

-- Apply function to txId list to get txs while predicate(either) holds, with save of first failed result
takeAssetTxsWhileWithResult :: NetworkId -> CurrencySymbol -> TokenName -> Maybe BfOrder -> (TxId -> IO a) -> (a -> Either l r) -> IO [Either l r]
takeAssetTxsWhileWithResult network cs name mbOrder getTxInfo predicate = concat <$> foldPagesWhile 1
    where
        foldPagesWhile page = Api.getAssetTxs network cs name mbOrder page >>= \case
            [] -> pure []
            txs -> checkTxsWhile (atrTxHash <$> txs) >>= \case
                [] -> pure []
                is -> if all isRight is then (is :) <$> foldPagesWhile (page + 1) else pure [is]
        checkTxsWhile = \case
            []     -> pure []
            i:is -> getTxInfo i <&> predicate >>= \case
                Right r -> (Right r :) <$> checkTxsWhile is
                Left  l -> pure [Left l]

--------------------------------------------------- Encoins distribution  ---------------------------------------------------

verifyAsset :: NetworkId -> CurrencySymbol -> TokenName -> Integer -> Address -> IO (Maybe TxId)
verifyAsset network cs token amount addr = do
    history <- getAssetTxs network cs token
    foldM (\res (atrTxHash -> txId) -> (res <|>) <$> (Api.getTxUtxo network txId <&> findOutput txId . turOutputs)) Nothing history
    where
        findOutput txId outs = const (Just txId) =<< find (\o -> turoAddress o == addr && P.valueOf (fromCardanoValue $ turoAmount o) cs token == amount) outs

verifyAssetFast
    :: NetworkId
    -> CurrencySymbol
    -> TokenName
    -> [(Address, Integer)]
    -> Maybe ([(Address, Integer, TxId)] -> IO ()) -- Function to save intermidiate results
    -> [(Address, Integer, TxId)]      -- Already verified addresses
    -> IO [Either Address (Address, Integer, TxId)]
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
            pairs <- map (\o -> (turoAddress o, P.valueOf (fromCardanoValue $ turoAmount o) cs token)) . turOutputs <$> Api.getTxUtxo network txId
            let res = map (\(a,b) -> (a,b,txId)) $ pairs `intersect` rs
                currentCounter = total - length rs
            liftIO $ maybe (pure ()) ($ res) saveIntermidiate
            liftIO $ putStrLn $ show currentCounter <> "/" <> show total
            (map Right res <>) <$> go (rs \\ pairs) hs

--------------------------------------------------- Helpers  ---------------------------------------------------

foldPages :: (Int -> IO [a]) -> IO [a]
foldPages getWithPage = go 1
    where go page = do
            res <- getWithPage page
            case res of
                [] -> pure []
                xs -> (xs <>) <$> go (page + 1)

mkEvaluatedMapUtxo :: NetworkId -> UtxoRequirements -> TxUtxoResponse -> IO MapUTXO
mkEvaluatedMapUtxo network reqs resp =  Map.fromList . mapMaybe sequence . zip (unspentTxOutRefs resp) <$>
    mapM (mkEvaluatedDecoratedTxOut network reqs) (turOutputs resp)

mkEvaluatedDecoratedTxOut :: NetworkId -> UtxoRequirements -> TxUtxoResponseOutput -> IO (Maybe DecoratedTxOut)
mkEvaluatedDecoratedTxOut network reqs TxUtxoResponseOutput{..} = sequence $ mkUnevaluatedDecoratedTxOut TxUtxoResponseOutput{..} <&> \txOut -> do
        fd <- evaluateWhen RequiresDatum     evaluateDatum      $ turoDatumHash <|> hashDatum  <$> turoInlineDatum
        fs <- evaluateWhen RequiresScript    evaluateScript      turoReferenceScriptHash
        fv <- evaluateWhen RequiresValidator evaluateValidator $ preview decoratedTxOutValidatorHash txOut
        pure $ fd . fs . fv $ txOut
    where
        evaluateWhen :: UtxoRequirement -> (v -> IO (DecoratedTxOut -> DecoratedTxOut)) -> Maybe v -> IO (DecoratedTxOut -> DecoratedTxOut)
        evaluateWhen req f mbV = case (req `Set.member` reqs, mbV) of
            (True, Just v) -> f v
            _              -> pure id

        evaluateDatum dh = do
            dfq <- case (dh == Datum.unitHash, turoInlineDatum) of
                (_   , Just iDat) -> pure $ DatumInline iDat
                (True,         _) -> pure $ DatumInBody $ Datum $ toBuiltinData ()
                _                 -> maybe DatumUnknown DatumInBody <$> getDatumByHash network dh
            pure (decoratedTxOutDatum .~ (dh, dfq))

        evaluateScript sh = do
            script <- getScriptByHash network sh
            pure (decoratedTxOutReferenceScript .~ script)

        evaluateValidator vh = do
            validator <- getValidatorByHash network vh
            pure (decoratedTxOutValidator .~ validator)

unspentTxOutRefs :: TxUtxoResponse -> [TxOutRef]
unspentTxOutRefs TxUtxoResponse{..} = TxOutRef turTxHash . turoIdx <$> turOutputs

mkUnevaluatedDecoratedTxOut :: TxUtxoResponseOutput -> Maybe DecoratedTxOut
mkUnevaluatedDecoratedTxOut TxUtxoResponseOutput{..} =
    let _decoratedTxOutStakingCredential = addressStakingCredential turoAddress
        _decoratedTxOutValue             = turoAmount
        _decoratedTxOutPubKeyDatum       = dat
        _decoratedTxOutReferenceScript   = Nothing
        _decoratedTxOutValidator         = Nothing
    in case addressCredential turoAddress of
        PubKeyCredential _decoratedTxOutPubKeyHash    -> pure PublicKeyDecoratedTxOut{..}
        ScriptCredential _decoratedTxOutValidatorHash -> do
            _decoratedTxOutScriptDatum <- _decoratedTxOutPubKeyDatum
            pure ScriptDecoratedTxOut{..}
    where
        dat = case (turoInlineDatum, turoDatumHash == Just Datum.unitHash) of
            (Just inlineDatum, _   ) -> Just (fromMaybe (hashDatum inlineDatum) turoDatumHash, DatumInline inlineDatum)
            (_               , True) -> Just (Datum.unitHash, DatumUnknown)
            _                        -> Nothing