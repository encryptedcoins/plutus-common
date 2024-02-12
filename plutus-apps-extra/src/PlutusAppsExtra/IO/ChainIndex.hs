{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module PlutusAppsExtra.IO.ChainIndex where

import qualified Cardano.Api                           as C
import           Control.Monad.IO.Class                (MonadIO (..))
import           Data.Kind                             (Constraint)
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes)
import           GHC.Base                              (Type)
import           Ledger                                (Address, DecoratedTxOut (..), TxOutRef, selectLovelace)
import           Plutus.Script.Utils.Ada               (Ada)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo    as Kupo
import           PlutusAppsExtra.IO.ChainIndex.Maestro (MonadMaestro)
import qualified PlutusAppsExtra.IO.ChainIndex.Maestro as Maestro
import qualified PlutusAppsExtra.IO.ChainIndex.Plutus  as Plutus
import           PlutusAppsExtra.Types.Tx              (UtxoRequirements)
import           PlutusAppsExtra.Utils.ChainIndex      (MapUTXO)

type HasChainIndex c m = (Monad m, IsChainIndex c, ChainIndexCostraints c m)

class IsChainIndex c where
    type ChainIndexCostraints c (m :: Type -> Type) :: Constraint
    type ChainIndexCostraints c m = MonadIO m
    getUtxosAt :: ChainIndexCostraints c m => UtxoRequirements -> Address -> m MapUTXO
    getUnspentTxOutFromRef :: ChainIndexCostraints c m => UtxoRequirements -> TxOutRef -> m (Maybe DecoratedTxOut)

data Plutus

instance IsChainIndex Plutus where
    getUtxosAt _ addr = liftIO $ Plutus.getUtxosAt addr
    getUnspentTxOutFromRef _ txOutRef = liftIO $ Plutus.getUnspentTxOutFromRef txOutRef

data Kupo

instance IsChainIndex Kupo where
    getUtxosAt reqs addr = liftIO $ Kupo.getUtxosAt reqs addr
    getUnspentTxOutFromRef reqs txOutRef = liftIO $ Kupo.getUnspentTxOutFromRef reqs txOutRef

data Maestro

instance IsChainIndex Maestro where
    type ChainIndexCostraints Maestro m = (MonadIO m, MonadMaestro m)
    getUtxosAt reqs addr = Maestro.getUtxosAt reqs addr
    getUnspentTxOutFromRef reqs txOutRef = Maestro.getUnspentTxOutFromRef reqs txOutRef

getRefsAt :: forall c m. HasChainIndex c m => Address -> m [TxOutRef]
getRefsAt addr = Map.keys <$> getUtxosAt @c @m mempty addr

getValueAt :: forall c m. HasChainIndex c m => Address -> m C.Value
getValueAt addr = mconcat . fmap _decoratedTxOutValue . Map.elems <$> getUtxosAt @c @m mempty addr

getAdaAt :: forall c m. HasChainIndex c m => Address -> m Ada
getAdaAt addr = fromIntegral . selectLovelace <$> getValueAt @c @m addr

getMapUtxoFromRefs :: forall c m. HasChainIndex c m => UtxoRequirements -> [TxOutRef] -> m MapUTXO
getMapUtxoFromRefs reqs = fmap (Map.fromList . catMaybes) . mapM (\input -> fmap (input,) <$> getUnspentTxOutFromRef @c @m reqs input)