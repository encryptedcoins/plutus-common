{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module PlutusAppsExtra.Test.Utils.Tx where

import           Cardano.Api                          (ConwayEra, TxMetadataInEra)
import           Cardano.Node.Emulator                (Params (..))
import           Control.Exception                    (try)
import           Control.Monad                        (when)
import           Control.Monad.State                  (MonadIO (..), StateT, evalStateT, execState, get, modify)
import           Data.Coerce                          (coerce)
import           Data.Either                          (isRight)
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromJust, isNothing, listToMaybe, mapMaybe)
import           Ledger                               (Address (..), CardanoTx (..), DecoratedTxOut (..), ScriptHash (..),
                                                       TxOutRef, ValidatorHash (..), adaValueOf, always, toCardanoValue)
import           PlutusAppsExtra.Constraints.Balance  (balanceExternalTx)
import           PlutusAppsExtra.Constraints.OffChain (useAsCollateralTx')
import           PlutusAppsExtra.IO.Time              (currentTime)
import           PlutusAppsExtra.Test.Utils.Gen       (genTxOutRef)
import           PlutusAppsExtra.Types.Error          (BalanceExternalTxError)
import           PlutusAppsExtra.Types.Tx             (TransactionBuilder, TxConstructor (..), buildTxConstraints, mkTxConstructor)
import           PlutusAppsExtra.Utils.ChainIndex     (MapUTXO)
import           PlutusAppsExtra.Utils.Datum          (inlinedUnitInTxOut)
import           PlutusLedgerApi.V1                   (Credential (..), TxId (..))
import           PlutusLedgerApi.V3                   (Value)
import qualified PlutusLedgerApi.V1                   as PV1
import qualified PlutusLedgerApi.V2                   as PV2
import qualified PlutusLedgerApi.V3                   as PV3
import qualified PlutusTx.AssocMap                    as AMap
import           PlutusTx.Builtins                    (emptyByteString)
import           PlutusTx.Numeric                     (AdditiveMonoid (..))
import           Test.Hspec                           (Expectation, expectationFailure, shouldSatisfy)
import           Test.QuickCheck                      (generate)

type TxTestM a = StateT TxTestState IO a

type TxTestState = MapUTXO

buildTx
    :: Params
    -> Maybe TxOutRef
    -> Address -> [TransactionBuilder ()]
    -> Maybe (TxMetadataInEra ConwayEra)
    -> TxTestM (Either BalanceExternalTxError CardanoTx)
buildTx pParams collateral addr txs mbMeta = get >>= \utxos -> liftIO $ do
    ct <- currentTime
    let constrInit = mkTxConstructor ct utxos
        txs' = map (useAsCollateralTx' collateral >>) txs
        lookupsAndCons = listToMaybe $ mapMaybe (`buildTxConstraints` constrInit) txs'
    when (isNothing lookupsAndCons) $ liftIO $ expectationFailure $ "AllConstructorsFailed:\n"
        <> show (concatMap (txConstructorErrors . (`execState` constrInit)) txs')
    let (lookups, cons) = fromJust lookupsAndCons
    try @BalanceExternalTxError $ balanceExternalTx pParams utxos addr lookups cons mbMeta

runTxTest :: TxTestM (Either BalanceExternalTxError CardanoTx) -> Expectation
runTxTest test = evalStateT test mempty >>= (`shouldSatisfy` isRight)

withAdaUtxo :: Integer -> Address -> TxTestM ()
withAdaUtxo i addr = do
    let val = adaValueOf $ fromInteger $ i * 1_000_000
        adaOut = case addr of
            (Address (PubKeyCredential pkh) mbSc) -> PublicKeyDecoratedTxOut pkh mbSc val Nothing Nothing
            (Address (ScriptCredential sh)  mbSc) -> ScriptDecoratedTxOut (coerce sh) mbSc val inlinedUnitInTxOut Nothing Nothing
    ref <- liftIO $ generate genTxOutRef
    modify (<> Map.fromList [(ref, adaOut)])

withValueUtxo :: Value -> Address -> TxTestM ()
withValueUtxo val addr = do
    let val' = either (error . show) id $ toCardanoValue val
        tokensOut = case addr of
            (Address (PubKeyCredential pkh) mbSc) -> PublicKeyDecoratedTxOut pkh mbSc val' Nothing Nothing
            (Address (ScriptCredential sh)  mbSc) -> ScriptDecoratedTxOut (coerce sh) mbSc val' inlinedUnitInTxOut Nothing Nothing
    ref <- liftIO $ generate genTxOutRef
    modify (<> Map.fromList [(ref, tokensOut)])

emptyTxInfoPV3 :: PV3.TxInfo
emptyTxInfoPV3 = PV3.TxInfo
    { PV3.txInfoInputs = []
    , PV3.txInfoOutputs = []
    , PV3.txInfoReferenceInputs = []
    , PV3.txInfoFee = zero
    , PV3.txInfoMint = zero
    , PV3.txInfoTxCerts = []
    , PV3.txInfoWdrl = AMap.empty
    , PV3.txInfoValidRange = always
    , PV3.txInfoSignatories = []
    , PV3.txInfoData = AMap.empty
    , PV3.txInfoRedeemers = AMap.empty
    , PV3.txInfoId = PV3.TxId emptyByteString
    , PV3.txInfoVotes = AMap.empty
    , PV3.txInfoProposalProcedures = []
    , PV3.txInfoCurrentTreasuryAmount = Nothing
    , PV3.txInfoTreasuryDonation = Nothing
    }

emptyTxInfoPV2 :: PV2.TxInfo
emptyTxInfoPV2 = PV2.TxInfo
    { PV2.txInfoInputs = []
    , PV2.txInfoOutputs = []
    , PV2.txInfoReferenceInputs = []
    , PV2.txInfoFee = zero
    , PV2.txInfoMint = zero
    , PV2.txInfoDCert = []
    , PV2.txInfoWdrl = AMap.empty
    , PV2.txInfoValidRange = always
    , PV2.txInfoSignatories = []
    , PV2.txInfoRedeemers = AMap.empty
    , PV2.txInfoData = AMap.empty
    , PV2.txInfoId = TxId emptyByteString
    }

emptyTxInfoPV1 :: PV1.TxInfo
emptyTxInfoPV1 = PV1.TxInfo
    { PV1.txInfoInputs = []
    , PV1.txInfoOutputs = []
    , PV1.txInfoFee = zero
    , PV1.txInfoMint = zero
    , PV1.txInfoDCert = []
    , PV1.txInfoWdrl = []
    , PV1.txInfoValidRange = always
    , PV1.txInfoSignatories = []
    , PV1.txInfoData = []
    , PV1.txInfoId = TxId emptyByteString
    }