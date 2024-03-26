module PlutusAppsExtra.IO.Tx.Blockfrost where

import           Control.Exception              (throw)
import           Data.Maybe                     (fromMaybe)
import           Ledger                         (CardanoTx, TxId)
import           PlutusAppsExtra.Api.Blockfrost (MonadBlockfrost)
import qualified PlutusAppsExtra.Api.Blockfrost as Blockfrost
import           PlutusAppsExtra.Types.Error    (MaestroError (..))
import           PlutusAppsExtra.Utils.Tx       (cardanoTxToText)
import qualified Text.Hex                       as T

-- Send a balanced transaction to Cardano Wallet Backend and return immediately
submitTx :: MonadBlockfrost m => CardanoTx -> m TxId
submitTx ctx = Blockfrost.submitTx $ fromMaybe (throw $ MaestroUnserialisableTx ctx) $ cardanoTxToText ctx >>= T.decodeHex