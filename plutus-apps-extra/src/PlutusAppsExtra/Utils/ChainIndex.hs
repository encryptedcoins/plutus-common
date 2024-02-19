{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module PlutusAppsExtra.Utils.ChainIndex where

import           Cardano.Node.Emulator.Params (Params (..))
import           Control.Monad.Catch          (MonadThrow, throwM)
import qualified Data.Map                     as Map
import           Ledger                       (DecoratedTxOut (..), TxOut, TxOutRef (..), adaOnlyValue, toTxOut)

import           PlutusAppsExtra.Types.Error  (MkTxError (..))

type MapUTXO = Map.Map TxOutRef DecoratedTxOut

toCardanoUtxo :: (MonadThrow m) => Params -> MapUTXO -> m (Map.Map TxOutRef TxOut)
toCardanoUtxo params utxos =
    let f (a, b) = (a, ) <$> either (throwM . UnbuildableTxOut b) pure (toTxOut (pNetworkId params) b)
    in Map.fromList <$> mapM f (Map.toList utxos)

filterPubKeyUtxos :: MapUTXO -> MapUTXO
filterPubKeyUtxos = Map.filter $ \case
   PublicKeyDecoratedTxOut {} -> True
   ScriptDecoratedTxOut {}    -> False

filterScriptUtxos :: MapUTXO -> MapUTXO
filterScriptUtxos = Map.filter $ \case
   PublicKeyDecoratedTxOut {} -> False
   ScriptDecoratedTxOut {}    -> True

filterCleanUtxos :: MapUTXO -> MapUTXO
filterCleanUtxos = Map.filter $ (\v -> adaOnlyValue v == v) . _decoratedTxOutValue