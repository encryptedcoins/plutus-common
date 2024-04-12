{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <&>" #-}

module PlutusAppsExtra.IO.Blockfrost where

import           Cardano.Api                      (StakeAddress, makeStakeAddress)
import           Cardano.Api.Shelley              (PoolId)
import           Control.Applicative              ((<|>))
import           Control.Lens                     (preview, (.~), (^.))
import           Control.Lens.Tuple               (Field3 (_3))
import           Control.Monad                    (foldM)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans              (MonadTrans (..))
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Data.Either                      (isRight, rights)
import           Data.Foldable                    (find)
import           Data.Functor                     ((<&>))
import           Data.List                        (intersect, (\\))
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set                         as Set
import           Ledger                           (Address (..), Datum (..), DatumFromQuery (..), DatumHash, DecoratedTxOut (..), Script,
                                                   ScriptHash, StakePubKeyHash (..), TxId (..), TxOutRef (..), Validator, ValidatorHash,
                                                   Versioned, decoratedTxOutDatum, decoratedTxOutReferenceScript, decoratedTxOutValidator,
                                                   decoratedTxOutValidatorHash, fromCardanoValue)
import           Plutus.Script.Utils.Value        (CurrencySymbol, TokenName)
import           Plutus.V1.Ledger.Api             (Credential (..), ToData (..))
import qualified Plutus.V1.Ledger.Value           as P
import           PlutusAppsExtra.Api.Blockfrost   (MonadBlockfrost (..))
import qualified PlutusAppsExtra.Api.Blockfrost   as Api
import           PlutusAppsExtra.Types.Tx         (UtxoRequirement (..), UtxoRequirements)
import           PlutusAppsExtra.Utils.Address    (spkhToStakeCredential)
import           PlutusAppsExtra.Utils.Blockfrost (AccDelegationHistoryResponse (..), AccountAssociatedAddressesResponse (..),
                                                   AddressDetailsResponse (..), AssetAddressesResponse (..), AssetHistoryResponse,
                                                   AssetTxsResponse (..), BfOrder (..), TxUtxoResponse (..), TxUtxoResponseInput (..),
                                                   TxUtxoResponseOutput (..), mkBfAddress)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import           PlutusAppsExtra.Utils.Datum      (hashDatum)
import qualified PlutusAppsExtra.Utils.Datum      as Datum
import           PlutusAppsExtra.Utils.Network    (HasNetworkId (..))
import           PlutusAppsExtra.Utils.Servant    (handle404Maybe)

----------------------------------------------------------- Hi-level API -----------------------------------------------------------

getAddressFromStakePubKeyHash :: MonadBlockfrost m => PoolId -> StakePubKeyHash -> m (Maybe Address)
getAddressFromStakePubKeyHash poolId spkh = runMaybeT $ do
    network <- lift getNetworkId
    history <- MaybeT $ sequence $ Api.getAccountDelegationHistory . makeStakeAddress network <$> spkhToStakeCredential spkh
    txHash  <- MaybeT $ pure $ adhrTxHash <$> find ((== poolId) . adhrPoolId) history
    MaybeT $ fmap turiAddress . listToMaybe . turInputs <$> Api.getTxUtxo txHash

getAddressDetails :: MonadBlockfrost m => Address -> m (Maybe AddressDetailsResponse)
getAddressDetails addr = handle404Maybe $ do
    network <- getNetworkId
    bfAddress <- mkBfAddress network addr
    Api.getAddressDetails bfAddress

getStakeAddressLastPool :: MonadBlockfrost m => StakeAddress -> m (Maybe PoolId)
getStakeAddressLastPool stakeAddr = fmap adhrPoolId . listToMaybe <$> Api.getAccountDelegationHistory stakeAddr

getAddressFromStakeAddress :: MonadBlockfrost m => StakeAddress -> m (Maybe Address)
getAddressFromStakeAddress stakeAddr = do
    txId <- fmap adhrTxHash . listToMaybe <$> Api.getAccountDelegationHistory stakeAddr
    maybe (pure Nothing) (fmap (fmap turiAddress . listToMaybe . turInputs) . Api.getTxUtxo) txId

getAccountAssociatedAddresses :: MonadBlockfrost m => StakePubKeyHash -> m (Maybe [Address])
getAccountAssociatedAddresses spkh = do
    network <- getNetworkId
    case makeStakeAddress network <$> spkhToStakeCredential spkh of
        Just sa -> Just . concatMap aaarAddresses <$> foldPages (fmap mkList . Api.getAccountAssociatedAddresses sa)
        Nothing -> pure Nothing
    where
        mkList = \case
            AccountAssociatedAddressesResponse [] -> []
            resp                                  -> [resp]

getAssetAddressess :: MonadBlockfrost m =>  CurrencySymbol -> TokenName -> m [(Address, Integer)]
getAssetAddressess cs name = fmap (map $ \AssetAddressesResponse{..} -> (adrAddress, adrQuantity))
    $ foldPages $ Api.getAssetAddresses cs name Nothing

getAssetHistory :: MonadBlockfrost m =>  CurrencySymbol -> TokenName -> m [AssetHistoryResponse]
getAssetHistory cs name = foldPages $ Api.getAssetHistory cs name Nothing

getAssetTxs :: MonadBlockfrost m =>  CurrencySymbol -> TokenName -> m [AssetTxsResponse]
getAssetTxs cs name = foldPages $ Api.getAssetTxs cs name Nothing

getTxUtxo :: MonadBlockfrost m => UtxoRequirements -> TxId -> m MapUTXO
getTxUtxo reqs txId = Api.getTxUtxo txId >>= mkEvaluatedMapUtxo reqs

isUsedAddress :: MonadBlockfrost m =>  Address -> m Bool
isUsedAddress addr = maybe False ((/= 0) . adrTxCount) <$> getAddressDetails addr

-------------------------------------------------------- Get by hash  --------------------------------------------------------

getDatumByHash :: MonadBlockfrost m =>  DatumHash -> m (Maybe Datum)
getDatumByHash = handle404Maybe . Api.getDatumByHashUnsafe

getScriptByHash :: MonadBlockfrost m =>  ScriptHash -> m (Maybe (Versioned Script))
getScriptByHash = handle404Maybe . Api.getScriptByHashUnsafe

getValidatorByHash :: MonadBlockfrost m =>  ValidatorHash -> m (Maybe (Versioned Validator))
getValidatorByHash = handle404Maybe . Api.getValidatorByHashUnsafe

-------------------------------------------- Iterative monitoring (delegated txs)  --------------------------------------------

-- Get ids of txs with asset of the specified policy
-- If txId is passed as the last argument - function will stop after ecnountering this txId
-- This function is useful when you need to iteratively monitor txs related to specific asset
getAllAssetTxsAfterTxId :: MonadBlockfrost m =>  CurrencySymbol -> TokenName -> Maybe TxId -> m [TxId]
getAllAssetTxsAfterTxId cs name mbTxId = takeAssetTxsWhile cs name (Just Desc) pure predicate
    where
        predicate txId = if Just txId == mbTxId then Left txId else Right txId

-- Apply function to txId list to get txs while predicate(either) holds
takeAssetTxsWhile ::  MonadBlockfrost m =>  CurrencySymbol -> TokenName -> Maybe BfOrder -> (TxId -> m a) -> (a -> Either l r) -> m [r]
takeAssetTxsWhile cs name mbOrder getTxInfo predicate = rights <$> takeAssetTxsWhileWithResult cs name mbOrder getTxInfo predicate

-- Apply function to txId list to get txs while predicate(either) holds, with save of first failed result
takeAssetTxsWhileWithResult :: MonadBlockfrost m =>  CurrencySymbol -> TokenName -> Maybe BfOrder -> (TxId -> m a) -> (a -> Either l r) -> m [Either l r]
takeAssetTxsWhileWithResult cs name mbOrder getTxInfo predicate = concat <$> foldPagesWhile 1
    where
        foldPagesWhile page = Api.getAssetTxs cs name mbOrder page >>= \case
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

verifyAsset :: MonadBlockfrost m => CurrencySymbol -> TokenName -> Integer -> Address -> m (Maybe TxId)
verifyAsset cs token amount addr = do
    history <- getAssetTxs cs token
    foldM (\res (atrTxHash -> txId) -> (res <|>) <$> (Api.getTxUtxo txId <&> findOutput txId . turOutputs)) Nothing history
    where
        findOutput txId outs = const (Just txId) =<< find (\o -> turoAddress o == addr && P.valueOf (fromCardanoValue $ turoAmount o) cs token == amount) outs

verifyAssetFast
    :: MonadBlockfrost m => CurrencySymbol
    -> TokenName
    -> [(Address, Integer)]
    -> Maybe ([(Address, Integer, TxId)] -> m ()) -- Function to save intermidiate results
    -> [(Address, Integer, TxId)]      -- Already verified addresses
    -> m [Either Address (Address, Integer, TxId)]
verifyAssetFast cs token recepients saveIntermidiate verified = do
        history <- getAssetTxs cs token
        go recepients $ filter ((`notElem` map (^. _3) verified) . atrTxHash) history
    where
        total = length recepients
        -- end of recepients list
        go [] _ = pure []
        -- end of token history
        go rs [] = pure $ map (Left . fst) rs
        go rs ((atrTxHash -> txId) : hs) = do
            pairs <- map (\o -> (turoAddress o, P.valueOf (fromCardanoValue $ turoAmount o) cs token)) . turOutputs <$> Api.getTxUtxo txId
            let res = map (\(a,b) -> (a,b,txId)) $ pairs `intersect` rs
                currentCounter = total - length rs
            maybe (pure ()) ($ res) saveIntermidiate
            liftIO $ putStrLn $ show currentCounter <> "/" <> show total
            (map Right res <>) <$> go (rs \\ pairs) hs

--------------------------------------------------- Helpers  ---------------------------------------------------

foldPages :: MonadBlockfrost m => (Int -> m [a]) -> m [a]
foldPages getWithPage = go 1
    where go page = do
            res <- getWithPage page
            case res of
                [] -> pure []
                xs -> (xs <>) <$> go (page + 1)

mkEvaluatedMapUtxo :: MonadBlockfrost m => UtxoRequirements -> TxUtxoResponse -> m MapUTXO
mkEvaluatedMapUtxo reqs resp =  Map.fromList . mapMaybe sequence . zip (unspentTxOutRefs resp) <$>
    mapM (mkEvaluatedDecoratedTxOut reqs) (turOutputs resp)

mkEvaluatedDecoratedTxOut :: MonadBlockfrost m => UtxoRequirements -> TxUtxoResponseOutput -> m (Maybe DecoratedTxOut)
mkEvaluatedDecoratedTxOut reqs TxUtxoResponseOutput{..} = sequence $ mkUnevaluatedDecoratedTxOut TxUtxoResponseOutput{..} <&> \txOut -> do
        fd <- evaluateWhen RequiresDatum     evaluateDatum      $ turoDatumHash <|> hashDatum  <$> turoInlineDatum
        fs <- evaluateWhen RequiresScript    evaluateScript      turoReferenceScriptHash
        fv <- evaluateWhen RequiresValidator evaluateValidator $ preview decoratedTxOutValidatorHash txOut
        pure $ fd . fs . fv $ txOut
    where
        evaluateWhen :: MonadBlockfrost m => UtxoRequirement -> (v -> m (DecoratedTxOut -> DecoratedTxOut)) -> Maybe v -> m (DecoratedTxOut -> DecoratedTxOut)
        evaluateWhen req f mbV = case (req `Set.member` reqs, mbV) of
            (True, Just v) -> f v
            _              -> pure id

        evaluateDatum dh = do
            dfq <- case (dh == Datum.unitHash, turoInlineDatum) of
                (_   , Just iDat) -> pure $ DatumInline iDat
                (True,         _) -> pure $ DatumInBody $ Datum $ toBuiltinData ()
                _                 -> maybe DatumUnknown DatumInBody <$> getDatumByHash dh
            pure (decoratedTxOutDatum .~ (dh, dfq))

        evaluateScript sh = do
            script <- getScriptByHash sh
            pure (decoratedTxOutReferenceScript .~ script)

        evaluateValidator vh = do
            validator <- getValidatorByHash vh
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