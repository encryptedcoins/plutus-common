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

import           Cardano.Api                              (BabbageEra, EraInMode (..), TxMetadataInEra (..))
import           Cardano.Node.Emulator                    (Params (..))
import           Cardano.Node.Emulator.Fee                (makeAutoBalancedTransactionWithUtxoProvider, utxoProviderFromWalletOutputs)
import           Control.Monad.Catch                      (MonadThrow (..))
import           Ledger                                   (Address, CardanoTx (..))
import           Ledger.Index                             (UtxoIndex (..))
import           Ledger.Tx.CardanoAPI                     (toCardanoAddressInEra)
import           Ledger.Typed.Scripts                     (Any, ValidatorTypes (..))
import           PlutusAppsExtra.PlutusApps               (ScriptLookups (..), TxConstraints, UnbalancedTx (..), mkTxWithParams)
import           PlutusAppsExtra.Types.Error              (BalanceExternalTxError (..), mkUnbalancedTxError, throwEither)
import           PlutusAppsExtra.Utils.ChainIndex         (MapUTXO, toCardanoUtxo)
import           PlutusAppsExtra.Utils.Tx                 (addMetadataToCardanoBuildTx)
import           Prelude

balanceExternalTx :: (MonadThrow m)
                  => Params
                  -> MapUTXO
                  -> Address
                  -> ScriptLookups Any
                  -> TxConstraints (RedeemerType Any) (DatumType Any)
                  -> Maybe (TxMetadataInEra BabbageEra)
                  -> m CardanoTx
balanceExternalTx params walletUTXO changeAddress lookups cons mbMetadata = do
    unbalancedTx@(UnbalancedCardanoTx cbt _) <- mkUnbalancedTx
    utxoIndex       <- UtxoIndex <$> toCardanoUtxo params (slTxOutputs lookups)
    cAddress        <- mkAddressInEra
    walletUTXOIndex <- toCardanoUtxo params walletUTXO
    let utxoProvider = either (throwM . MakeUtxoProviderError unbalancedTx) pure . utxoProviderFromWalletOutputs walletUTXOIndex
        cbtWithMeta = addMetadataToCardanoBuildTx mbMetadata cbt
    (`CardanoTx` BabbageEraInCardanoMode) <$> makeAutoBalancedTransactionWithUtxoProvider
        params
        utxoIndex
        cAddress
        utxoProvider
        (throwM . MakeAutoBalancedTxError unbalancedTx)
        cbtWithMeta
  where
    mkUnbalancedTx = either (throwM . mkUnbalancedTxError lookups cons) pure $ mkTxWithParams params lookups cons
    mkAddressInEra = throwEither (NonBabbageEraChangeAddress changeAddress) $ toCardanoAddressInEra (pNetworkId params) changeAddress