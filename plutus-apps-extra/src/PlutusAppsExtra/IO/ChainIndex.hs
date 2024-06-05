{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}

module PlutusAppsExtra.IO.ChainIndex where

import qualified Cardano.Api                           as C
import           Control.Monad.IO.Class                (MonadIO (..))
import           Data.Aeson                            (FromJSON, ToJSON)
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes)
import           GHC.Generics                          (Generic)
import           Ledger                                (Address, DecoratedTxOut (..), TxOutRef, selectLovelace)
import           Plutus.Script.Utils.Ada               (Ada)
import           PlutusAppsExtra.Api.Maestro           (MonadMaestro)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo    as Kupo
import qualified PlutusAppsExtra.IO.ChainIndex.Maestro as Maestro
import           PlutusAppsExtra.Types.Tx              (UtxoRequirements)
import           PlutusAppsExtra.Utils.ChainIndex      (MapUTXO)

class Monad m => HasChainIndexProvider m where

    getChainIndexProvider :: m ChainIndexProvider

    getUtxosAt :: UtxoRequirements -> Address -> m MapUTXO
    default getUtxosAt :: MonadMaestro m => UtxoRequirements -> Address -> m MapUTXO
    getUtxosAt reqs addr = getChainIndexProvider >>= \case
        Kupo    -> liftIO $ Kupo.getUtxosAt reqs addr
        Maestro -> Maestro.getUtxosAt reqs addr

    getUnspentTxOutFromRef :: UtxoRequirements -> TxOutRef -> m (Maybe DecoratedTxOut)
    default getUnspentTxOutFromRef :: MonadMaestro m => UtxoRequirements -> TxOutRef -> m (Maybe DecoratedTxOut)
    getUnspentTxOutFromRef reqs txOutRef = getChainIndexProvider >>= \case
        Kupo    -> liftIO $ Kupo.getUnspentTxOutFromRef reqs txOutRef
        Maestro -> Maestro.getUnspentTxOutFromRef reqs txOutRef

data ChainIndexProvider = Kupo | Maestro
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

getRefsAt :: HasChainIndexProvider m => Address -> m [TxOutRef]
getRefsAt addr = Map.keys <$> getUtxosAt mempty addr

getValueAt :: HasChainIndexProvider m => Address -> m C.Value
getValueAt addr = mconcat . fmap _decoratedTxOutValue . Map.elems <$> getUtxosAt mempty addr

getAdaAt :: HasChainIndexProvider m => Address -> m Ada
getAdaAt addr = fromIntegral . selectLovelace <$> getValueAt addr

getMapUtxoFromRefs :: HasChainIndexProvider m => UtxoRequirements -> [TxOutRef] -> m MapUTXO
getMapUtxoFromRefs reqs = fmap (Map.fromList . catMaybes) . mapM (\input -> fmap (input,) <$> getUnspentTxOutFromRef reqs input)