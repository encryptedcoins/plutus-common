{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module PlutusAppsExtra.IO.Node where

import           Cardano.Api.Shelley                                 (CardanoMode, ConsensusModeParams (..), EpochSlots (..),
                                                                      LocalChainSyncClient (NoLocalChainSyncClient),
                                                                      LocalNodeClientProtocols (..), LocalNodeConnectInfo (..),
                                                                      NetworkId, TxInMode (..), TxValidationErrorInMode,
                                                                      connectToLocalNode)
import qualified Cardano.Node.Client                                 as CLient
import           Control.Concurrent.STM                              (atomically, newEmptyTMVarIO, putTMVar, takeTMVar)
import           Control.Monad.Catch                                 (Exception (fromException), handle, throwM)
import           GHC.IO.Exception                                    (IOErrorType (NoSuchThing), IOException (..))
import           Ledger.Tx                                           (CardanoTx (..))
import           Network.HTTP.Client                                 (HttpExceptionContent, Request)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx
import           PlutusAppsExtra.Types.Error                         (ConnectionError, SubmitTxToLocalNodeError (..))
import           PlutusAppsExtra.Utils.Servant                       (Endpoint, getFromEndpointOnPort,
                                                                      pattern ConnectionErrorOnPort)
import qualified Servant.API                                         as Sevant

healthCheck :: IO Sevant.NoContent
healthCheck = getFromEndpointCardanoNode CLient.healthcheck

getFromEndpointCardanoNode :: Endpoint a
getFromEndpointCardanoNode = getFromEndpointOnPort 3003

pattern CardanoNodeConnectionError :: Request -> HttpExceptionContent -> ConnectionError
pattern CardanoNodeConnectionError req content <- ConnectionErrorOnPort 3003 req content

sumbitTxToNodeLocal
    :: FilePath
    -> NetworkId
    -> CardanoTx
    -> IO (Net.Tx.SubmitResult (TxValidationErrorInMode CardanoMode))
sumbitTxToNodeLocal socketPath networkId (CardanoTx tx eraInMode) = handleConnectionAbscence $ do
        resultVar <- newEmptyTMVarIO
        _ <- connectToLocalNode
            connctInfo
            LocalNodeClientProtocols
                { localChainSyncClient    = NoLocalChainSyncClient
                , localTxSubmissionClient = Just (localTxSubmissionClientSingle resultVar)
                , localStateQueryClient   = Nothing
                , localTxMonitoringClient = Nothing
                }
        atomically (takeTMVar resultVar) >>= \case
            SubmitSuccess -> pure SubmitSuccess
            SubmitFail reason -> throwM $ FailedSumbit reason
    where
        connctInfo = LocalNodeConnectInfo
            { localConsensusModeParams = CardanoModeParams $ EpochSlots 0
            , localNodeNetworkId       = networkId
            , localNodeSocketPath      = socketPath
            }
        localTxSubmissionClientSingle resultVar =
            Net.Tx.LocalTxSubmissionClient
            $ pure $ Net.Tx.SendMsgSubmitTx (TxInMode tx eraInMode) $ \result -> do
                atomically $ putTMVar resultVar result
                pure (Net.Tx.SendMsgDone ())

handleConnectionAbscence :: IO a -> IO a
handleConnectionAbscence = handle $ \e -> case fromException e of
    Just IOError{ioe_type = NoSuchThing} -> throwM NoConnectionToLocalNode
    _ -> throwM e