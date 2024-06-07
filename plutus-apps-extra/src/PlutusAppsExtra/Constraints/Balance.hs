{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.Constraints.Balance where

import           Cardano.Api                         (ConwayEra, ShelleyBasedEra (ShelleyBasedEraConway), TxMetadataInEra (..))
import           Cardano.Node.Emulator               (Params (..))
import           Cardano.Node.Emulator.Internal.Node (Params (..), makeAutoBalancedTransactionWithUtxoProvider,
                                                      utxoProviderFromWalletOutputs)
import           Control.Monad.Catch                 (MonadThrow (..))
import           Ledger                              (Address, CardanoTx (..), fromDecoratedIndex)
import           Ledger.Tx.CardanoAPI                (toCardanoAddressInEra)
import           Ledger.Typed.Scripts                (Any, ValidatorTypes (..))
import           PlutusAppsExtra.PlutusApps          (ScriptLookups (..), TxConstraints, UnbalancedTx (UnbalancedCardanoTx), mkTxWithParams)
import           PlutusAppsExtra.Types.Error         (BalanceExternalTxError (..), mkUnbalancedTxError, throwEither)
import           PlutusAppsExtra.Utils.ChainIndex    (MapUTXO)
import           PlutusAppsExtra.Utils.Tx            (addMetadataToCardanoBuildTx)
import           Prelude

balanceExternalTx :: (MonadThrow m)
                  => Params
                  -> MapUTXO
                  -> Address
                  -> ScriptLookups Any
                  -> TxConstraints (RedeemerType Any) (DatumType Any)
                  -> Maybe (TxMetadataInEra ConwayEra)
                  -> m CardanoTx
balanceExternalTx params walletUTXO changeAddress lookups cons mbMetadata = do
    (UnbalancedCardanoTx cbt _) <- mkUnbalancedTx
    utxoIndex       <- either (throwM . UtxoIndexConversionError) pure $ fromDecoratedIndex networkId $ (walletUTXO <>) $ slTxOutputs lookups
    cAddress        <- mkAddressInEra
    let utxoProvider = either (throwM . MakeUtxoProviderError) pure
                     . utxoProviderFromWalletOutputs utxoIndex cbtWithMeta
        cbtWithMeta = addMetadataToCardanoBuildTx mbMetadata cbt
    (`CardanoTx` ShelleyBasedEraConway) <$> makeAutoBalancedTransactionWithUtxoProvider
        params
        utxoIndex
        cAddress
        utxoProvider
        (throwM . MakeAutoBalancedTxError)
        cbtWithMeta
  where
    networkId = pNetworkId params
    mkUnbalancedTx = either (throwM . mkUnbalancedTxError lookups cons) pure $ mkTxWithParams params lookups cons
    mkAddressInEra = throwEither (NonBabbageEraChangeAddress changeAddress) $ toCardanoAddressInEra networkId changeAddress
