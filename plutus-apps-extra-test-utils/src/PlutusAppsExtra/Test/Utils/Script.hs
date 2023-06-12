{-# LANGUAGE FlexibleContexts #-}

module PlutusAppsExtra.Test.Utils.Script where

import           Cardano.Api.Shelley                               (ProtocolParameters (protocolParamProtocolVersion))
import           Cardano.Node.Emulator                             (Params (..), pProtocolParams)
import           Codec.Serialise                                   (serialise)
import           Control.Monad.Except                              (MonadError (..))
import           Data.Bifunctor                                    (Bifunctor (..))
import qualified Data.ByteString.Lazy                              as LBS
import qualified Data.ByteString.Short                             as SBS
import           Data.Either                                       (isRight)
import           Plutus.V2.Ledger.Api                              (CostModelApplyError (CMInternalReadError), Data,
                                                                    EvaluationContext, EvaluationError, ExBudget, LogOutput,
                                                                    MintingPolicy (..), ProtocolVersion (ProtocolVersion), Script,
                                                                    ScriptContext, Validator (..), VerboseMode (..),
                                                                    evaluateScriptCounting, mkEvaluationContext, toData, ToData)
import           PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCostModelParams)
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
    ctx <- getEvalCtx
    return $ evaluateScriptCounting 
        (protocolVersionFromParams p) 
        v 
        ctx 
        (SBS.toShort . LBS.toStrict $ serialise script) 
        plutusData

getEvalCtx :: Either CostModelApplyError EvaluationContext
getEvalCtx = case defaultCostModelParams of
    Nothing -> throwError CMInternalReadError
    Just n  -> mkEvaluationContext n

runScriptQuiet, runScriptVerbose :: Params -> Script -> [Data] -> Either CostModelApplyError (LogOutput, Either EvaluationError ExBudget)
runScriptQuiet   = runScript Quiet
runScriptVerbose = runScript Verbose

protocolVersionFromParams :: Params -> ProtocolVersion
protocolVersionFromParams = uncurry ProtocolVersion . bimap fromIntegral fromIntegral . protocolParamProtocolVersion . pProtocolParams