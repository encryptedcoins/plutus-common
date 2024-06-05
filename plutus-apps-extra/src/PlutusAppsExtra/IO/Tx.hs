{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.IO.Tx where

import           Cardano.Address.Style.Shelley       (getKey)
import           Cardano.Api                         (BabbageEra, TxMetadataInEra)
import           Cardano.Node.Emulator               (Params)
import           Control.Monad                       (void)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, ToJSON)
import qualified Data.Map                            as Map
import           GHC.Generics                        (Generic)
import           Ledger                              (CardanoTx (..), DecoratedTxOut (..), PaymentPubKeyHash (PaymentPubKeyHash),
                                                      PubKeyHash, StakingCredential, TxOutRef, _decoratedTxOutAddress, fromCardanoValue,
                                                      getCardanoTxOutputs, toPlutusAddress, txOutAddress, txOutValue)
import qualified Ledger
import           Ledger.Tx.CardanoAPI                (unspentOutputsTx)
import           Ledger.Typed.Scripts                (Any, ValidatorTypes (..))
import qualified Plutus.Script.Utils.Ada             as Ada
import           Plutus.Script.Utils.Value           (leq)
import qualified Plutus.V2.Ledger.Api                as P
import           PlutusAppsExtra.Api.Maestro         (MonadMaestro)
import           PlutusAppsExtra.Constraints.Balance (balanceExternalTx)
import           PlutusAppsExtra.IO.ChainIndex       (HasChainIndexProvider)
import qualified PlutusAppsExtra.IO.Node             as Node
import qualified PlutusAppsExtra.IO.Tx.Cardano       as Cardano
import qualified PlutusAppsExtra.IO.Tx.Maestro       as Maestro
import           PlutusAppsExtra.IO.Wallet           (HasWalletProvider (..), getWalletKeys, getWalletUtxos, wkPaymentKey)
import           PlutusAppsExtra.PlutusApps          (ScriptLookups, TxConstraints, mustPayToPubKey, mustPayToPubKeyAddress)
import           PlutusAppsExtra.Utils.ChainIndex    (MapUTXO)
import           PlutusAppsExtra.Utils.Network       (HasNetworkId (getNetworkId))
import           PlutusTx.Prelude                    (zero, (-))
import           Prelude                             hiding ((-))

data TxProvider = Cardano FilePath | Maestro
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

isCardanoTxProvider :: TxProvider -> Bool
isCardanoTxProvider = \case
    Cardano _ -> True
    _         -> False

class (HasWalletProvider m, HasChainIndexProvider m) => HasTxProvider m where

    getTxProvider :: m TxProvider

    signTx :: CardanoTx -> m CardanoTx
    signTx ctx = getTxProvider >>= \case
        Cardano{} -> Cardano.signTx ctx
        Maestro   -> signTxWithoutNode ctx

    submitTx :: CardanoTx -> m ()
    default submitTx :: MonadMaestro m => CardanoTx -> m ()
    submitTx ctx = getTxProvider >>= \case
       Cardano{} -> Cardano.submitTx ctx
       Maestro   -> void $ Maestro.submitTx ctx

    awaitTxConfirmed :: CardanoTx -> m ()
    default awaitTxConfirmed :: MonadMaestro m => CardanoTx -> m ()
    awaitTxConfirmed ctx = getTxProvider >>= \case
       Cardano{} -> Cardano.awaitTxConfirmed ctx
       Maestro   -> Maestro.awaitTxConfirmed ctx

-- | Send a balanced transaction to local cardano node if `txProvider` is `cardano` or use `sumbitTx` otherwise
sumbitTxToNodeLocal :: HasTxProvider m => CardanoTx -> m ()
sumbitTxToNodeLocal ctx = getTxProvider >>= \case
    Cardano nodeFp -> do
        networkId <- getNetworkId
        void $ liftIO $ Node.sumbitTxToNodeLocal nodeFp networkId ctx
    _ -> submitTx ctx

signTxWithoutNode :: HasWalletProvider m => CardanoTx -> m CardanoTx
signTxWithoutNode ctx = flip Ledger.addCardanoTxSignature ctx <$> (getKey .  wkPaymentKey <$> getWalletKeys)

balanceTx :: HasTxProvider m
    => Params
    -> ScriptLookups Any
    -> TxConstraints (RedeemerType Any) (DatumType Any)
    -> Maybe (TxMetadataInEra BabbageEra)
    -> m CardanoTx
balanceTx params lookups cons mbMetadata = do
    changeAddress <- getWalletAddr
    walletUTXO <- getWalletUtxos mempty
    balanceExternalTx params walletUTXO changeAddress lookups cons mbMetadata

-- | Send a balanced transaction and wait until transaction is confirmed or declined
submitTxConfirmed :: HasTxProvider m => CardanoTx -> m ()
submitTxConfirmed ctx = submitTx ctx >> awaitTxConfirmed ctx

-- | Get wallet total profit from a transaction
getTxProfit :: HasWalletProvider m => CardanoTx -> MapUTXO -> m P.Value
getTxProfit tx txUtxos = do
        addrs <- getWalletAddresses
        let txOuts   = Map.elems txUtxos
            spent    = getTotalValue addrs _decoratedTxOutValue _decoratedTxOutAddress txOuts
            produced = getTotalValue addrs txOutValue (toPlutusAddress . txOutAddress) $ getCardanoTxOutputs tx
        pure $ fromCardanoValue produced - fromCardanoValue spent
    where
        getTotalValue addrs getValue getAddr = mconcat . map getValue . filter ((`elem` addrs) . getAddr)

isProfitableTx :: HasWalletProvider m => CardanoTx -> MapUTXO -> m Bool
isProfitableTx tx txUtxos = leq zero <$> getTxProfit tx txUtxos

-- | Create and submit a transaction that produces a specific number of outputs at the target wallet address
getWalletTxOutRefs :: HasTxProvider m => Params -> PubKeyHash -> Maybe StakingCredential -> Int -> m [TxOutRef]
getWalletTxOutRefs params pkh mbSkc n = do
    liftIO $ putStrLn "Balancing..."
    balancedTx <- balanceTx params lookups cons Nothing
    liftIO $ print balancedTx
    liftIO $ putStrLn "Signing..."
    signedTx <- signTx balancedTx
    liftIO $ print signedTx
    liftIO $ putStrLn "Submitting..."
    submitTxConfirmed signedTx
    let refs = Map.keys $ unspentOutputsTx signedTx
    liftIO $ putStrLn "Submitted!"
    return refs
    where
        lookups = mempty :: ScriptLookups Any
        cons    = case mbSkc of
            Just skc -> mconcat $ replicate n $ mustPayToPubKeyAddress (PaymentPubKeyHash pkh) skc $ Ada.lovelaceValueOf 10_000_000
            Nothing  -> mustPayToPubKey (PaymentPubKeyHash pkh) $ Ada.lovelaceValueOf 10_000_000