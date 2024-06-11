{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.IO.Tx.Cardano where

import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (unless, when)
import           Control.Monad.Catch            (MonadThrow (..))
import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Default                   (Default (..))
import           Data.Maybe                     (listToMaybe)
import           Data.Word                      (Word64)
import           Ledger                         (Slot (..))
import           PlutusAppsExtra.Api.Kupo       (CreatedOrSpent (..), KupoRequest (..), SpentOrUnspent (..), getKupoResponseWithHeaders)
import           PlutusAppsExtra.IO.Tx.Internal (AwaitTxParameters (..))
import           PlutusAppsExtra.Types.Error    (SubmitTxError (..))
import           PlutusAppsExtra.Utils.Kupo     (KupoResponse (..), MkPattern (..), SlotWithHeaderHash (..))
import qualified PlutusLedgerApi.V1             as PV1
import           Servant.API                    (ResponseHeader (..), getResponse)
import           Servant.API.ResponseHeaders    (lookupResponseHeader)

-- ------------------------------------------- Tx functions -------------------------------------------

awaitTxConfirmed :: (MonadThrow m, MonadIO m) => AwaitTxParameters -> PV1.TxId -> m ()
awaitTxConfirmed MkAwaitTxParameters{..} txId = go 0
  where
    -- We don't require for only @unspent@. Kupo with @--prune-utxo@ option would still keep spent UTxOs until their spent record is truly immutable (see Kupo docs for more details).
    req :: KupoRequest SUUnspent CSCreated CSCreated
    req = def {reqPattern = mkPattern txId}
    go :: (MonadThrow m, MonadIO m) => Int -> m ()
    go attempt = do
        when (attempt >= maxAttempts) $ throwM $ AwaiTxMaxAttemptsExceeded txId
        responses <- liftIO $ getKupoResponseWithHeaders req
        let mbResponse = listToMaybe (getResponse responses)
            rcHeader   = lookupResponseHeader responses :: ResponseHeader "X-Most-Recent-Checkpoint" Word64
        case (mbResponse, rcHeader) of

            (Nothing, _)
                -> liftIO (threadDelay checkInterval) >> go (attempt + 1)

            (Just  r, Header slotOfCurrentBlock)
                -> unless (fromInteger (getSlot $ swhhSlot $ krCreatedAt r) + slotsToWait <= slotOfCurrentBlock)
                $ liftIO $ threadDelay checkInterval >> go (attempt + 1)

            (_      , _) -> error "Header 'X-Most-Recent-Checkpoint' isn't seen in response"

    slotsToWait = 3 * confirmations * 20
