{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}

module PlutusAppsExtra.IO.ChainIndex where

import qualified Cardano.Api                          as C
import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Aeson                           (FromJSON)
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes)
import           GHC.Generics                         (Generic)
import           Ledger                               (Address, DecoratedTxOut (..), TxOutRef, selectLovelace)
import           Plutus.Script.Utils.Ada              (Ada)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo   as Kupo
import qualified PlutusAppsExtra.IO.ChainIndex.Plutus as Plutus
import           PlutusAppsExtra.Utils.ChainIndex     (MapUTXO)

data ChainIndex = Plutus | Kupo
    deriving (Show, Generic, FromJSON)

class MonadIO m => HasChainIndex m where
    getChainIndex :: m ChainIndex

getRefsAt :: HasChainIndex m => Address -> m [TxOutRef]
getRefsAt addr = Map.keys <$> getUtxosAt addr

getUtxosAt :: HasChainIndex m => Address -> m MapUTXO
getUtxosAt addr = getChainIndex >>= \case
    Plutus -> liftIO $ Plutus.getUtxosAt addr
    Kupo   -> liftIO $   Kupo.getUtxosAt addr

getValueAt :: HasChainIndex m => Address -> m C.Value
getValueAt addr = mconcat . fmap _decoratedTxOutValue . Map.elems <$> getUtxosAt addr

getAdaAt :: HasChainIndex m => Address -> m Ada
getAdaAt addr = fromIntegral . selectLovelace <$> getValueAt addr

getUnspentTxOutFromRef :: HasChainIndex m => TxOutRef -> m (Maybe DecoratedTxOut)
getUnspentTxOutFromRef txOutRef = getChainIndex >>= \case
    Plutus -> liftIO $ Plutus.getUnspentTxOutFromRef txOutRef
    Kupo   -> liftIO $   Kupo.getUnspentTxOutFromRef txOutRef

getMapUtxoFromRefs :: HasChainIndex m => [TxOutRef] -> m MapUTXO
getMapUtxoFromRefs = fmap (Map.fromList . catMaybes) . mapM (\input -> fmap (input,) <$> getUnspentTxOutFromRef input)