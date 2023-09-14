{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.Constraints.Balance where

import           Cardano.Api                      (EraInMode (..))
import           Cardano.Node.Emulator            (Params (..))
import           Cardano.Node.Emulator.Fee        (makeAutoBalancedTransactionWithUtxoProvider, utxoProviderFromWalletOutputs)
import           Control.Monad.Catch              (MonadThrow (..))
import           Ledger                           (Address, CardanoTx (..))
import           Ledger.Tx.Constraints            (ScriptLookups (..), TxConstraints, UnbalancedTx (..), mkTxWithParams)
import           Ledger.Index                     (UtxoIndex (..))
import           Ledger.Tx.CardanoAPI             (toCardanoAddressInEra)
import           Ledger.Typed.Scripts             (Any, ValidatorTypes (..))
import           PlutusAppsExtra.Types.Error      (BalanceExternalTxError (..), throwEither)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO, toCardanoUtxo)
import           Prelude

balanceExternalTx :: (MonadThrow m)
                  => Params
                  -> MapUTXO
                  -> Address
                  -> ScriptLookups Any
                  -> TxConstraints (RedeemerType Any) (DatumType Any)
                  -> m CardanoTx
balanceExternalTx params walletUTXO changeAddress lookups cons = do
    UnbalancedCardanoTx cbt _ <- throwEither MakeUnbalancedTxError $ mkTxWithParams params lookups cons
    utxoIndex                 <- UtxoIndex <$> toCardanoUtxo params (slTxOutputs lookups)
    cAddress                  <- throwEither NonBabbageEraChangeAddress $ toCardanoAddressInEra (pNetworkId params) changeAddress
    walletUTXOIndex           <- toCardanoUtxo params walletUTXO
    let utxoProvider = either (throwM . MakeUtxoProviderError) pure . utxoProviderFromWalletOutputs walletUTXOIndex
    (`CardanoTx` BabbageEraInCardanoMode) <$> makeAutoBalancedTransactionWithUtxoProvider
        params
        utxoIndex
        cAddress
        utxoProvider
        (throwM . MakeAutoBalancedTxError)
        cbt