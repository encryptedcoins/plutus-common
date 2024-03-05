{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <&>"          #-}

module PlutusAppsExtra.IO.Tx.Maestro where

import           Control.Concurrent             (threadDelay)
import           Control.Exception              (throw)
import           Control.Monad.Catch            (throwM)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Functor                   ((<&>))
import           Data.Maybe                     (fromMaybe)
import           Ledger                         (CardanoTx, getCardanoTxId)
import           Ledger.Tx                      (TxId)
import           PlutusAppsExtra.Api.Maestro    (MonadMaestro)
import           PlutusAppsExtra.IO.Maestro     (getTxState)
import qualified PlutusAppsExtra.IO.Maestro     as Maestro
import           PlutusAppsExtra.IO.Tx.Internal (TxState (Onchain, Pending))
import           PlutusAppsExtra.Types.Error    (MaestroError (..), MkTxError (..))
import           PlutusAppsExtra.Utils.Maestro  (TxStateResponse (..))
import           PlutusAppsExtra.Utils.Tx       (cardanoTxToText)
import qualified Text.Hex                       as T

-- Send a balanced transaction to Cardano Wallet Backend and return immediately
submitTx :: MonadMaestro m => CardanoTx -> m TxId
submitTx ctx = Maestro.sumbitTx $ fromMaybe (throw $ MaestroUnserialisableTx ctx) $ cardanoTxToText ctx >>= T.decodeHex

-- Wait until a transaction is confirmed (added to the ledger).
-- If the transaction is never added to the ledger then 'awaitTxConfirmed' never
-- returns
awaitTxConfirmed :: MonadMaestro m => CardanoTx -> m ()
awaitTxConfirmed (getCardanoTxId -> txId) = go
    where
        go = do
            getTxState txId <&> fmap tsrState >>= \case
                Nothing      -> go
                Just Onchain -> pure ()
                Just Pending -> liftIO (threadDelay 1_000_000) >> go
                Just state   -> throwM (FailedToSubmit state)