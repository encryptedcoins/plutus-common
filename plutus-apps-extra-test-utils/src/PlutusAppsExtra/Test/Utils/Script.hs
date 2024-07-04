{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusAppsExtra.Test.Utils.Script where

import           Cardano.Api.Shelley                               (ProtocolParameters (protocolParamProtocolVersion))
import           Cardano.Ledger.Alonzo.Core                        (eraProtVerLow)
import           Cardano.Ledger.Alonzo.Plutus.Context              (PlutusScriptContext)
import           Cardano.Ledger.Era                                (Era)
import           Cardano.Ledger.Plutus                             (Plutus (..), PlutusBinary (..), PlutusLanguage (..))
import qualified Cardano.Ledger.Plutus.Language                    as Plutus
import           Cardano.Node.Emulator                             (Params (..))
import           Cardano.Node.Emulator.Internal.Node               (pProtocolParams)
import           Data.Bifunctor                                    (Bifunctor (..))
import           Data.Data                                         (Proxy (..))
import           Data.Either                                       (isRight)
import           Ledger                                            (MintingPolicy (..), Script (..), Validator (..))
import           PlutusCore.Builtin.Debug                          (BuiltinSemanticsVariant (..))
import           PlutusCore.Data                                   (Data)
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as PLC
import qualified PlutusLedgerApi.Common                            as LedgerApi
import           PlutusLedgerApi.Common.Versions                   (MajorProtocolVersion, allegraPV, alonzoPV, conwayPV, maryPV, shelleyPV,
                                                                    valentinePV, vasilPV)
import           PlutusLedgerApi.V3                                (CostModelApplyError (..), EvaluationContext, EvaluationError, ExBudget,
                                                                    LogOutput, ProtocolVersion (..), VerboseMode (..))
import           PlutusTx                                          (ToData (..), toData)
import           Test.Hspec                                        (Expectation, HasCallStack, expectationFailure, shouldSatisfy)

testScipt :: forall (lang :: Plutus.Language) era.
    ( HasCallStack
    , Plutus.PlutusLanguage lang
    , Era era
    ) => Script -> [Data] -> Expectation
testScipt script plutusData = case runScript @lang @era Verbose script plutusData of
    Left err                 -> expectationFailure $ show err
    Right (output, Left err) -> expectationFailure $ show err <> "\nOutput:\n" <> show output
    Right (_, res)           -> res `shouldSatisfy` isRight

testMintingPolicy :: forall (lang :: Plutus.Language) era redeemer.
    ( HasCallStack
    , Plutus.PlutusLanguage lang
    , Era era
    , ToData redeemer
    , ToData (PlutusScriptContext lang)
    ) => MintingPolicy -> redeemer -> PlutusScriptContext lang -> Expectation
testMintingPolicy (MintingPolicy mp) red sctx = testScipt @lang @era mp [toData red, toData sctx]

testValidator :: forall (lang :: Plutus.Language) era datum redeemer.
    ( HasCallStack
    , Plutus.PlutusLanguage lang
    , Era era
    , ToData datum
    , ToData redeemer
    , ToData (PlutusScriptContext lang)
    ) => Validator -> datum -> redeemer -> PlutusScriptContext lang -> Expectation
testValidator (Validator v) dat red sctx = testScipt @lang @era v [toData dat, toData red, toData sctx]

--------------------------------------------------------------------------------

runScript :: forall (lang :: Plutus.Language) era. (HasCallStack, Plutus.PlutusLanguage lang, Era era)
    => VerboseMode -> Script -> [Data] -> Either CostModelApplyError (LogOutput, Either EvaluationError ExBudget)
runScript v (Script s) plutusData = do
    let lang = Plutus.plutusLanguage (Proxy @lang)
        script' = either (error . show) id $ decodePlutusRunnable @lang (eraProtVerLow @era) $ Plutus $ PlutusBinary s
    return $ Plutus.evaluatePlutusRunnableBudget
        @lang
        (eraProtVerLow @era)
        v
        (mkEvalCtx lang)
        script'
        plutusData

-- | Create the evaluation context for the benchmarks. This doesn't exactly match how it's done
-- on-chain, but that's okay because the evaluation context is cached by the ledger, so we're
-- deliberately not including it in the benchmarks.
mkEvalCtx :: Plutus.Language -> EvaluationContext
mkEvalCtx lang =
    case PLC.defaultCostModelParams of
        Just p ->
            let errOrCtx =
                    -- The validation benchmarks were all created from PlutusV1 scripts
                    LedgerApi.mkDynEvaluationContext
                        (toLedgerLang lang)
                        [DefaultFunSemanticsVariant1]
                        (const DefaultFunSemanticsVariant1)
                        p
            in case errOrCtx of
                Right ec -> ec
                Left err -> error $ show err
        Nothing -> error "Couldn't get cost model params"


runScriptQuiet, runScriptVerbose ::  forall (lang :: Plutus.Language) era.
    ( HasCallStack
    , Plutus.PlutusLanguage lang
    , Era era
    ) => Script -> [Data] -> Either CostModelApplyError (LogOutput, Either EvaluationError ExBudget)
runScriptQuiet   = runScript @lang @era Quiet
runScriptVerbose = runScript @lang @era Verbose

-- protocolVersionFromParams :: Params -> ProtocolVersion
-- protocolVersionFromParams = uncurry ProtocolVersion . bimap fromIntegral fromIntegral . protocolParamProtocolVersion . pProtocolParams

-- unsafeMajorProtocolVersionFromParams :: Params -> MajorProtocolVersion
-- unsafeMajorProtocolVersionFromParams p = case pvMajor $ protocolVersionFromParams p of
--     2 -> shelleyPV
--     3 -> allegraPV
--     4 -> maryPV
--     5 -> alonzoPV
--     7 -> vasilPV
--     8 -> valentinePV
--     9 -> conwayPV
--     _ -> error "unknown major protocol version"

toLedgerLang :: Plutus.Language -> LedgerApi.PlutusLedgerLanguage
toLedgerLang = \case
    Plutus.PlutusV1 -> LedgerApi.PlutusV1
    Plutus.PlutusV2 -> LedgerApi.PlutusV2
    Plutus.PlutusV3 -> LedgerApi.PlutusV3
