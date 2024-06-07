{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module PlutusAppsExtra.IO.Node where

import qualified Cardano.Api                                         as Api
import qualified Cardano.Api                                         as C
import           Cardano.Api.Shelley                                 (ConsensusModeParams (..), EpochSlots (..),
                                                                      LocalChainSyncClient (NoLocalChainSyncClient),
                                                                      LocalNodeClientProtocols (..), LocalNodeConnectInfo (..), NetworkId,
                                                                      TxInMode (..), connectToLocalNode)
import           Control.Concurrent.STM                              (atomically, newEmptyTMVarIO, putTMVar, takeTMVar)
import           Control.Monad.Catch                                 (Exception (fromException), MonadCatch, handle, throwM)
import           Control.Monad.IO.Class                              (MonadIO (..))
import           Data.Data                                           (Proxy (..))
import           Data.Maybe                                          (fromMaybe)
import           Data.String                                         (IsString (..))
import           GHC.IO.Exception                                    (IOErrorType (NoSuchThing), IOException (..))
import           Ledger.Tx                                           (CardanoTx (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx
import           PlutusAppsExtra.Types.Error                         (SubmitTxError (..))
import           PlutusAppsExtra.Utils.Network                       (networkIdToEpochSlots)
import           PlutusAppsExtra.Utils.Servant                       (getFromEndpointOnPort)
import           Servant.API                                         (Get, NoContent, OctetStream, (:>))
import qualified Servant.Client                                      as Servant

sumbitTxToNodeLocal
    :: (MonadIO m, MonadCatch m)
    => FilePath
    -> NetworkId
    -> CardanoTx
    -> m (Api.TxId, Net.Tx.SubmitResult C.TxValidationErrorInCardanoMode)
sumbitTxToNodeLocal socketPath networkId (CardanoTx tx eraInMode) = handleConnectionAbscence $ do
        resultVar <- liftIO newEmptyTMVarIO
        _ <- liftIO $ connectToLocalNode
            connctInfo
            LocalNodeClientProtocols
                { localChainSyncClient    = NoLocalChainSyncClient
                , localTxSubmissionClient = Just (localTxSubmissionClientSingle resultVar)
                , localStateQueryClient   = Nothing
                , localTxMonitoringClient = Nothing
                }
        liftIO (atomically $ takeTMVar resultVar) >>= \case
            C.SubmitSuccess     -> pure (txId, C.SubmitSuccess)
            C.SubmitFail reason -> throwM $ FailedSumbit reason
    where
        txId = Api.getTxId $ Api.getTxBody tx
        connctInfo = LocalNodeConnectInfo
            { localConsensusModeParams = CardanoModeParams $ fromMaybe (EpochSlots 0) $ networkIdToEpochSlots networkId
            , localNodeNetworkId       = networkId
            , localNodeSocketPath      = fromString socketPath
            }
        localTxSubmissionClientSingle resultVar =
            Net.Tx.LocalTxSubmissionClient
            $ pure $ Net.Tx.SendMsgSubmitTx (TxInMode eraInMode tx) $ \result -> do
                atomically $ putTMVar resultVar result
                pure (Net.Tx.SendMsgDone ())

handleConnectionAbscence :: MonadCatch m => m a -> m a
handleConnectionAbscence = handle $ \e -> case fromException e of
    Just err@IOError{ioe_type = NoSuchThing} -> throwM $ NoConnectionToLocalNode err
    _                                        -> throwM e

-- Node healthcheck returns ```InternalException Network.Socket.recvBuf: resource vanished (Connection reset by peer)```
-- so this functions uses node metrics instead
nodeHealthCheck :: IO NoContent
nodeHealthCheck = getFromEndpointOnPort nodeDiagnosticsPort $ Servant.client (Proxy @Metrics)

type Metrics = "metrics" :> Get '[OctetStream] NoContent

nodeDiagnosticsPort :: Int
nodeDiagnosticsPort = 12798