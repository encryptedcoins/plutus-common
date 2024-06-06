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
import           Cardano.Api                         (BabbageEra, SigningKey (..), TxMetadataInEra, ShelleyWitnessSigningKey (..))
import           Cardano.Node.Emulator               (Params)
import           Control.Monad                       (void)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Functor                        ((<&>))
import qualified Data.Map                            as Map
import           GHC.Generics                        (Generic)
import           Ledger                              (CardanoTx (..), DecoratedTxOut (..), PaymentPubKeyHash (PaymentPubKeyHash),
                                                      PubKeyHash, StakingCredential, TxOutRef, _decoratedTxOutAddress,
                                                      fromCardanoValue, getCardanoTxOutputs, getCardanoTxId, toPlutusAddress, 
                                                      txOutAddress, txOutValue)
import qualified Ledger
import           Ledger.Tx.CardanoAPI                (fromCardanoTxId, unspentOutputsTx)
import           Ledger.Typed.Scripts                (Any, ValidatorTypes (..))
import qualified Plutus.Script.Utils.Ada             as Ada
import           Plutus.Script.Utils.Value           (leq)
import qualified Plutus.V2.Ledger.Api                as P
import           PlutusAppsExtra.Api.Maestro         (MonadMaestro)
import           PlutusAppsExtra.Constraints.Balance (balanceExternalTx)
import           PlutusAppsExtra.IO.ChainIndex       (HasChainIndexProvider)
import qualified PlutusAppsExtra.IO.Node             as Node
import qualified PlutusAppsExtra.IO.Tx.Cardano       as Cardano
import           PlutusAppsExtra.IO.Tx.Internal      (AwaitTxParameters (..))
import qualified PlutusAppsExtra.IO.Tx.Maestro       as Maestro
import           PlutusAppsExtra.IO.Wallet           (HasWallet (..), getWalletAddr, getWalletKeys, getWalletUtxos, wkPaymentKey)
import           PlutusAppsExtra.PlutusApps          (ScriptLookups, TxConstraints, mustPayToPubKey, mustPayToPubKeyAddress)
import           PlutusAppsExtra.Utils.ChainIndex    (MapUTXO)
import           PlutusAppsExtra.Utils.Network       (HasNetworkId (getNetworkId))
import           PlutusTx.Prelude                    (zero, (-))
import           Prelude                             hiding ((-))

data TxProvider
    = Cardano FilePath AwaitTxParameters
    | Maestro AwaitTxParameters
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

isCardanoTxProvider :: TxProvider -> Bool
isCardanoTxProvider = \case
    Cardano{} -> True
    _         -> False

class (HasWallet m, HasChainIndexProvider m) => HasTxProvider m where

    getTxProvider :: m TxProvider

    submitTx :: CardanoTx -> m Ledger.TxId
    default submitTx :: MonadMaestro m => CardanoTx -> m Ledger.TxId
    submitTx ctx = getTxProvider >>= \case

        Cardano fp _ -> fst <$> do
            networkId <- getNetworkId
            void $ Node.sumbitTxToNodeLocal fp networkId ctx

        Maestro{}    -> Maestro.submitTx ctx

    awaitTxConfirmed :: P.TxId -> m ()
    default awaitTxConfirmed :: MonadMaestro m => P.TxId -> m ()
    awaitTxConfirmed txId = getTxProvider >>= \case
       Cardano{} -> getTxAwaitTxParameters >>= flip Cardano.awaitTxConfirmed txId
       Maestro{} -> Maestro.awaitTxConfirmed (_ txId)

getTxAwaitTxParameters :: HasTxProvider m => m AwaitTxParameters
getTxAwaitTxParameters = getTxProvider <&> \case
    Cardano _ ps -> ps
    Maestro ps   -> ps

signTx :: HasWallet m => CardanoTx -> m CardanoTx
signTx ctx = getWalletKeys <&>
    (`addCardanoTxWitness` ctx) . WitnessPaymentExtendedKey . PaymentExtendedSigningKey . getKey . wkPaymentKey
  where
    addCardanoTxWitness = undefined

balanceTx ::
    ( HasWallet m
    , HasChainIndexProvider m
    )
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
submitTxConfirmed ctx = submitTx ctx >> awaitTxConfirmed (getCardanoTxId ctx)

-- | Get wallet total profit from a transaction
getTxProfit :: HasWallet m => CardanoTx -> MapUTXO -> m P.Value
getTxProfit tx txUtxos = do
        addrs <- getWalletAddresses
        let txOuts   = Map.elems txUtxos
            spent    = getTotalValue addrs _decoratedTxOutValue _decoratedTxOutAddress txOuts
            produced = getTotalValue addrs txOutValue (toPlutusAddress . txOutAddress) $ getCardanoTxOutputs tx
        pure $ fromCardanoValue produced - fromCardanoValue spent
    where
        getTotalValue addrs getValue getAddr = mconcat . map getValue . filter ((`elem` addrs) . getAddr)

isProfitableTx :: HasWallet m => CardanoTx -> MapUTXO -> m Bool
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
