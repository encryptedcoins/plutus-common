{-# LANGUAGE FlexibleContexts #-}

module PlutusAppsExtra.Test.Utils.Script where

import           Cardano.Api.Shelley                               (ProtocolParameters (protocolParamProtocolVersion))
import           Cardano.Node.Emulator                             (Params (..), pProtocolParams)
import           Control.Monad.Except                              (MonadError (..))
import           Codec.Serialise                                   (serialise)
import           Data.Bifunctor                                    (Bifunctor (..))
import           Data.Either                                       (isRight)
import           Plutus.V1.Ledger.Api                              (CostModelApplyError (CMInternalReadError), Data,
                                                                    EvaluationContext, EvaluationError, ExBudget, LogOutput,
                                                                    ProtocolVersion (ProtocolVersion),
                                                                    VerboseMode (..), evaluateScriptCounting, mkEvaluationContext, Script)
import           PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCostModelParams)
import           Test.Hspec                                        (Expectation, expectationFailure, shouldSatisfy)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

testScipt :: Params -> Script -> [Data] -> Expectation
testScipt p script plutusData = case runScriptVerbose p script plutusData of
    Left err                 -> expectationFailure $ show err
    Right (output, Left err) -> expectationFailure $ show err <> "\nOutput:\n" <> show output
    Right (_, res)           -> res `shouldSatisfy` isRight 

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