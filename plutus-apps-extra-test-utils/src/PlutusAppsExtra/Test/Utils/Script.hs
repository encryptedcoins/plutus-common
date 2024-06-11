{-# LANGUAGE FlexibleContexts #-}

module PlutusAppsExtra.Test.Utils.Script where

import           Cardano.Api.Shelley                               (ProtocolParameters (protocolParamProtocolVersion))
import           Cardano.Node.Emulator                             (Params (..))
import           Cardano.Node.Emulator.Internal.Node               (pProtocolParams)
import           Codec.Serialise                                   (serialise)
import           Data.Bifunctor                                    (Bifunctor (..))
import qualified Data.ByteString.Lazy                              as LBS
import qualified Data.ByteString.Short                             as SBS
import           Data.Either                                       (isRight)
import           Ledger                                            (MintingPolicy (..), Script, ScriptContext, Validator (..))
import           PlutusCore.Builtin.Debug                          (BuiltinSemanticsVariant (..))
import           PlutusCore.Data                                   (Data)
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as PLC
import qualified PlutusLedgerApi.Common                            as LedgerApi
import           PlutusLedgerApi.Common.Versions                   (MajorProtocolVersion, allegraPV, alonzoPV, conwayPV, maryPV, shelleyPV,
                                                                    valentinePV, vasilPV)
import           PlutusLedgerApi.V3                                (CostModelApplyError (..), EvaluationContext, EvaluationError, ExBudget,
                                                                    LogOutput, ProtocolVersion (..), VerboseMode (..), deserialiseScript,
                                                                    evaluateScriptCounting, toData)
import           PlutusTx                                          (ToData)
import           Test.Hspec                                        (Expectation, expectationFailure, shouldSatisfy)

testScipt :: Params -> Script -> [Data] -> Expectation
testScipt p script plutusData = case runScriptVerbose p script plutusData of
    Left err                 -> expectationFailure $ show err
    Right (output, Left err) -> expectationFailure $ show err <> "\nOutput:\n" <> show output
    Right (_, res)           -> res `shouldSatisfy` isRight

testMintingPolicy :: (ToData redeemer) => Params -> MintingPolicy -> redeemer -> ScriptContext -> Expectation
testMintingPolicy p (MintingPolicy mp) red sctx = testScipt p mp [toData red, toData sctx]

testValidator :: (ToData datum, ToData redeemer) => Params -> Validator -> datum -> redeemer -> ScriptContext -> Expectation
testValidator p (Validator v) dat red sctx = testScipt p v [toData dat, toData red, toData sctx]

--------------------------------------------------------------------------------

runScript :: VerboseMode -> Params -> Script -> [Data] -> Either CostModelApplyError (LogOutput, Either EvaluationError ExBudget)
runScript v p script plutusData = do
    let mpv = unsafeMajorProtocolVersionFromParams p
        script' = either (error . show) id $ deserialiseScript mpv $ SBS.toShort $ LBS.toStrict $ serialise script
    return $ evaluateScriptCounting
        mpv
        v
        mkEvalCtx
        script'
        plutusData

-- | Create the evaluation context for the benchmarks. This doesn't exactly match how it's done
-- on-chain, but that's okay because the evaluation context is cached by the ledger, so we're
-- deliberately not including it in the benchmarks.
mkEvalCtx :: EvaluationContext
mkEvalCtx =
    case PLC.defaultCostModelParams of
        Just p ->
            let errOrCtx =
                    -- The validation benchmarks were all created from PlutusV1 scripts
                    LedgerApi.mkDynEvaluationContext
                        LedgerApi.PlutusV3
                        [DefaultFunSemanticsVariant1]
                        (const DefaultFunSemanticsVariant1)
                        p
            in case errOrCtx of
                Right ec -> ec
                Left err -> error $ show err
        Nothing -> error "Couldn't get cost model params"


runScriptQuiet, runScriptVerbose :: Params -> Script -> [Data] -> Either CostModelApplyError (LogOutput, Either EvaluationError ExBudget)
runScriptQuiet   = runScript Quiet
runScriptVerbose = runScript Verbose

protocolVersionFromParams :: Params -> ProtocolVersion
protocolVersionFromParams = uncurry ProtocolVersion . bimap fromIntegral fromIntegral . protocolParamProtocolVersion . pProtocolParams

unsafeMajorProtocolVersionFromParams :: Params -> MajorProtocolVersion
unsafeMajorProtocolVersionFromParams p = case pvMajor $ protocolVersionFromParams p of
    2 -> shelleyPV
    3 -> allegraPV
    4 -> maryPV
    5 -> alonzoPV
    7 -> vasilPV
    8 -> valentinePV
    9 -> conwayPV
    _ -> error "unknown major protocol version"
