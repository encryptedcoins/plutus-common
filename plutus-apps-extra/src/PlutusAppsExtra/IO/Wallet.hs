{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.IO.Wallet where

import           Cardano.Mnemonic                                   (MkSomeMnemonic (..), SomeMnemonic)
import           Cardano.Node.Emulator                              (Params)
import qualified Cardano.Wallet.Api.Client                          as Client
import           Cardano.Wallet.Api.Types                           (ApiSerialisedTransaction (..),
                                                                     ApiSignTransactionPostData (ApiSignTransactionPostData),
                                                                     ApiT (..), ApiTxId (..), ApiWallet)
import           Cardano.Wallet.Api.Types.SchemaMetadata            (TxMetadataSchema (..))
import           Cardano.Wallet.LocalClient.ExportTx                (export)
import           Cardano.Wallet.Primitive.AddressDerivation         (WalletKey (digest, publicKey))
import           Cardano.Wallet.Primitive.AddressDerivation.Shelley (generateKeyFromSeed)
import           Cardano.Wallet.Primitive.Passphrase                (Passphrase (..), currentPassphraseScheme, preparePassphrase)
import           Cardano.Wallet.Primitive.Types                     (WalletId (WalletId))
import           Cardano.Wallet.Primitive.Types.Address             (AddressState (..))
import           Control.Concurrent                                 (threadDelay)
import           Control.Lens                                       ((<&>), (^.), (^?))
import           Control.Monad                                      (unless, void)
import           Control.Monad.Catch                                (MonadThrow (..))
import           Control.Monad.Extra                                (concatMapM)
import           Control.Monad.IO.Class                             (MonadIO (..))
import           Data.Aeson                                         (FromJSON (..), ToJSON (..), eitherDecode, withObject, (.:))
import           Data.Aeson.Lens                                    (_String, key)
import qualified Data.ByteString.Lazy                               as LB
import           Data.Coerce                                        (coerce)
import qualified Data.Map                                           as Map
import           Data.Maybe                                         (mapMaybe)
import           Data.String                                        (IsString (..))
import           Data.Text                                          (Text)
import qualified Data.Text                                          as T
import           Data.Text.Class                                    (FromText (fromText))
import           Data.Void                                          (Void)
import           GHC.Generics                                       (Generic)
import           Ledger                                             (Address, CardanoTx (..), DecoratedTxOut (..),
                                                                     PaymentPubKeyHash (PaymentPubKeyHash), PubKeyHash,
                                                                     StakingCredential, TxOutRef, _decoratedTxOutAddress,
                                                                     decoratedTxOutPlutusValue, fromCardanoValue,
                                                                     getCardanoTxOutputs, toPlutusAddress, txOutAddress,
                                                                     txOutValue)
import           Ledger.Tx                                          (getCardanoTxId)
import           Ledger.Tx.CardanoAPI                               (unspentOutputsTx)
import           Ledger.Typed.Scripts                               (ValidatorTypes (..))
import           Network.HTTP.Client                                (HttpExceptionContent, Request)
import           Plutus.Script.Utils.Value                          (leq)
import           PlutusAppsExtra.IO.ChainIndex                      (HasChainIndex (..), getRefsAt, getUtxosAt)
import           PlutusTx.IsData                                    (FromData, ToData)
import           PlutusTx.Prelude                                   (zero, (-))
import           Prelude                                            hiding ((-))

import           Ledger.Tx.Constraints                              (ScriptLookups, TxConstraints, mkTxWithParams,
                                                                     mustPayToPubKey, mustPayToPubKeyAddress)
import qualified Plutus.Script.Utils.Ada                            as Ada
import qualified Plutus.Script.Utils.Ada                            as P
import qualified Plutus.V2.Ledger.Api                               as P
import           PlutusAppsExtra.Types.Error                        (ConnectionError, MkTxError (..), WalletError (..),
                                                                     throwEither, throwMaybe)
import           PlutusAppsExtra.Utils.Address                      (addressToKeyHashes, bech32ToAddress)
import           PlutusAppsExtra.Utils.ChainIndex                   (MapUTXO)
import           PlutusAppsExtra.Utils.Servant                      (Endpoint, getFromEndpointOnPort,
                                                                     pattern ConnectionErrorOnPort)
import           PlutusAppsExtra.Utils.Tx                           (apiSerializedTxToCardanoTx, cardanoTxToSealedTx)

------------------------------------------- Restore-wallet -------------------------------------------

class (Monad m, MonadIO m, MonadThrow m) => HasWallet m where
    getRestoredWallet :: m RestoredWallet

data RestoredWallet = RestoredWallet
    { name             :: Text
    , mnemonicSentence :: SomeMnemonic
    , passphrase       :: Passphrase "user"
    } deriving (Show, Eq, Generic)

instance FromJSON RestoredWallet where
    parseJSON = withObject "Restore wallet" $ \v -> do
        let mkMnemonic = either (fail . show) pure . mkSomeMnemonic @'[ 24 ]
            mkPassphrase = Passphrase . fromString
        name                   <- v .: "name"
        mnemonicSentence       <- v .: "mnemonic_sentence" >>= mkMnemonic
        passphrase             <- v .: "passphrase"        <&> mkPassphrase
        pure RestoredWallet{..}

genWalletId :: SomeMnemonic -> Passphrase "user" -> WalletId
genWalletId mnemonic pp = WalletId $ digest $ publicKey rootXPrv
  where
    rootXPrv = generateKeyFromSeed (mnemonic, Nothing) pwdP
    pwdP = preparePassphrase currentPassphraseScheme pp

restoreWalletFromFile :: (MonadIO m, MonadThrow m) => FilePath -> m RestoredWallet
restoreWalletFromFile fp = liftIO (LB.readFile fp) >>=
    either (throwM . RestoredWalletParsingError . T.pack) pure . eitherDecode

-- Read restore-wallet JSON file and generate walletId from it
walletIdFromFile :: (MonadIO m, MonadThrow m) => FilePath -> m WalletId
walletIdFromFile fp = do
    RestoredWallet{..} <- restoreWalletFromFile fp
    pure $ genWalletId mnemonicSentence passphrase

getWalletId :: HasWallet m => m WalletId
getWalletId = do
    RestoredWallet{..} <- getRestoredWallet
    pure $ genWalletId mnemonicSentence passphrase

------------------------------------------- Wallet functions -------------------------------------------

getFromEndpointWallet :: Endpoint a
getFromEndpointWallet = getFromEndpointOnPort 8090

pattern WalletApiConnectionError :: Request -> HttpExceptionContent -> ConnectionError
pattern WalletApiConnectionError req content <- ConnectionErrorOnPort 8090 req content

getWalletAddrBech32 :: HasWallet m => m Text
getWalletAddrBech32 = do
    walletId <- getWalletId
    getFromEndpointWallet (Client.listAddresses  Client.addressClient (ApiT walletId) (Just $ ApiT Unused)) >>= \case
        v:_ -> pure $ v ^. key "id"._String
        _   -> throwM $ WalletIdDoesntHaveAnyAssociatedAddresses walletId

getWalletAddr :: HasWallet m => m Address
getWalletAddr = do
    addrWalletBech32 <- getWalletAddrBech32
    case bech32ToAddress <$> fromText addrWalletBech32 of
        Right (Just addr) -> pure addr
        _                 -> throwM $ UnparsableAddress addrWalletBech32

getWalletKeyHashes :: HasWallet m => m (PubKeyHash, Maybe StakingCredential)
getWalletKeyHashes = do
    addrWallet <- getWalletAddr
    case addressToKeyHashes addrWallet of
        Just hs -> pure hs
        Nothing -> throwM $ AddressDoesntCorrespondToPubKey addrWallet

getWalletFromId :: HasWallet m => WalletId -> m ApiWallet
getWalletFromId = getFromEndpointWallet . Client.getWallet Client.walletClient . ApiT

ownAddresses :: HasWallet m => m [Address]
ownAddresses = mapMaybe bech32ToAddress <$> ownAddressesBech32

ownAddressesBech32 :: HasWallet m => m [Text]
ownAddressesBech32 = do
    walletId <- getWalletId
    as <- getFromEndpointWallet $ Client.listAddresses  Client.addressClient (ApiT walletId) Nothing
    pure $ map (^. key "id"._String) as

-- Get all value at a wallet
getWalletValue :: (HasWallet m, HasChainIndex m) => m P.Value
getWalletValue = mconcat . fmap decoratedTxOutPlutusValue . Map.elems <$> getWalletUtxos

-- Get all ada at a wallet
getWalletAda :: (HasWallet m, HasChainIndex m) => m P.Ada
getWalletAda = Ada.fromValue <$> getWalletValue

getWalletRefs :: (HasWallet m, HasChainIndex m) => m [TxOutRef]
getWalletRefs = ownAddresses >>= concatMapM getRefsAt

-- Get all utxos at a wallet
getWalletUtxos :: (HasWallet m, HasChainIndex m) => m MapUTXO
getWalletUtxos = ownAddresses >>= mapM getUtxosAt <&> mconcat

-- Get wallet total profit from a transaction.
getTxProfit :: HasWallet m => CardanoTx -> MapUTXO -> m P.Value
getTxProfit tx txUtxos = do
        addrs <- ownAddresses
        let txOuts   = Map.elems txUtxos
            spent    = getTotalValue addrs _decoratedTxOutValue _decoratedTxOutAddress txOuts
            produced = getTotalValue addrs txOutValue (toPlutusAddress . txOutAddress) $ getCardanoTxOutputs tx
        pure $ fromCardanoValue produced - fromCardanoValue spent
    where
        getTotalValue addrs getValue getAddr = mconcat . map getValue . filter ((`elem` addrs) . getAddr)

isProfitableTx :: HasWallet m => CardanoTx -> MapUTXO -> m Bool
isProfitableTx tx txUtxos = leq zero <$> getTxProfit tx txUtxos

------------------------------------------- Tx functions -------------------------------------------

signTx :: HasWallet m => CardanoTx -> m CardanoTx
signTx ctx = do
    ppUser   <- passphrase <$> getRestoredWallet
    walletId <- getWalletId
    asTx     <- sign walletId (coerce ppUser)
    throwMaybe (ConvertApiSerialisedTxToCardanoTxError asTx) $ apiSerializedTxToCardanoTx asTx
    where
        stx = cardanoTxToSealedTx ctx
        sign walletId pp = getFromEndpointWallet $ Client.signTransaction Client.transactionClient
            (ApiT walletId)
            (ApiSignTransactionPostData (ApiT stx) (ApiT pp))

balanceTx ::
    ( HasWallet m
    , FromData (DatumType a)
    , ToData (DatumType a)
    , ToData (RedeemerType a)
    ) =>
    Params -> ScriptLookups a -> TxConstraints (RedeemerType a) (DatumType a) -> m CardanoTx
balanceTx params lookups cons = do
    walletId     <- getWalletId
    unbalancedTx <- throwEither UnbuildableUnbalancedTx (mkTxWithParams params lookups cons)
    exportTx     <- throwEither UnbuildableExportTx $ export params unbalancedTx
    asTx         <- getFromEndpointWallet $ Client.balanceTransaction Client.transactionClient
        (ApiT walletId)
        (toJSON exportTx)
    throwMaybe (ConvertApiSerialisedTxToCardanoTxError asTx) $ apiSerializedTxToCardanoTx asTx

-- Send a balanced transaction to Cardano Wallet Backend and return immediately
submitTx :: HasWallet m => CardanoTx -> m ()
submitTx ctx = do
    let stx = cardanoTxToSealedTx ctx
    walletId <- getWalletId
    void $ getFromEndpointWallet $
        Client.submitTransaction Client.transactionClient
            (ApiT walletId)
            (ApiSerialisedTransaction $ ApiT stx)

-- Send a balanced transaction to Cardano Wallet Backend and wait until transaction is confirmed or declined
submitTxConfirmed :: HasWallet m => CardanoTx -> m ()
submitTxConfirmed ctx = submitTx ctx >> awaitTxConfirmed ctx

-- Wait until a transaction is confirmed (added to the ledger).
-- If the transaction is never added to the ledger then 'awaitTxConfirmed' never
-- returns
awaitTxConfirmed :: HasWallet m => CardanoTx -> m ()
awaitTxConfirmed ctx = go
    where
        go = do
            walletId <- getWalletId
            hash <- throwEither (CantExtractHashFromCardanoTx ctx) . fromText . T.pack . show  $ getCardanoTxId ctx
            res <- getFromEndpointWallet $ Client.getTransaction Client.transactionClient
                (ApiT walletId)
                (ApiTxId $ ApiT hash)
                TxMetadataNoSchema
            unless (confirmedResponse res) $ liftIO (threadDelay 1_000_000) >> go
        confirmedResponse res = case res ^? key "status"._String of
            Just "in_ledger" -> True
            _                -> False

-- Create and submit a transaction that produces a specific number of outputs at the target wallet address
getWalletTxOutRefs :: HasWallet m => Params -> PubKeyHash -> Maybe StakingCredential -> Int -> m [TxOutRef]
getWalletTxOutRefs params pkh mbSkc n = do
    liftIO $ putStrLn "Balancing..."
    balancedTx <- balanceTx params lookups cons
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
        lookups = mempty :: ScriptLookups Void
        cons    = case mbSkc of
            Just skc -> mconcat $ replicate n $ mustPayToPubKeyAddress (PaymentPubKeyHash pkh) skc $ Ada.lovelaceValueOf 10_000_000
            Nothing -> mustPayToPubKey (PaymentPubKeyHash pkh) $ Ada.lovelaceValueOf 10_000_000