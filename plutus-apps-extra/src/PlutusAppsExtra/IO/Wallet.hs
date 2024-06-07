{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.IO.Wallet
    ( getWalletKeyHashes
    , genPrvKey
    , genPubKey
    , getWalletValue
    , getWalletAda
    , getWalletRefs
    , getWalletUtxos
    , mkSignature
    , Internal.HasWallet (..)
    , Internal.RestoredWallet (..)
    , Internal.restoreWalletFromFile
    , Internal.WalletKeys (..)
    , Internal.getWalletKeys
    , Internal.getPassphrase
    ) where

import           Cardano.Address.Derivation         (XPrv)
import           Control.Lens                       ((<&>))
import           Control.Monad.Catch                (MonadThrow (..))
import           Control.Monad.Extra                (concatMapM)
import qualified Data.ByteArray                     as BA
import qualified Data.ByteString                    as BS
import qualified Data.List.NonEmpty                 as NonEmpty
import qualified Data.Map                           as Map
import           Ledger                             (Address, Passphrase (..), PubKey, PubKeyHash, Signature, StakingCredential, TxOutRef,
                                                     decoratedTxOutPlutusValue, generateFromSeed, toPublicKey)
import qualified Ledger                             as Caradano
import           Ledger.Crypto                      (signTx)
import qualified Plutus.Script.Utils.Ada            as Ada
import qualified Plutus.Script.Utils.Ada            as P
import           PlutusAppsExtra.IO.ChainIndex      (HasChainIndexProvider, getRefsAt, getUtxosAt)
import           PlutusAppsExtra.IO.Wallet.Internal (HasWallet (..), RestoredWallet (..))
import qualified PlutusAppsExtra.IO.Wallet.Internal as Internal
import           PlutusAppsExtra.Types.Error        (WalletError (..))
import           PlutusAppsExtra.Types.Tx           (UtxoRequirements)
import           PlutusAppsExtra.Utils.Address      (addressToKeyHashes)
import           PlutusAppsExtra.Utils.ChainIndex   (MapUTXO)
import qualified PlutusLedgerApi.V1                 as PV1
import qualified PlutusLedgerApi.V3                 as P
import           Prelude                            hiding ((-))
import           System.Random                      (genByteString, getStdGen)

getWalletAddr :: HasWallet m => m Address
getWalletAddr = NonEmpty.head <$> getWalletAddresses

getWalletKeyHashes :: HasWallet m => m (PubKeyHash, Maybe StakingCredential)
getWalletKeyHashes = do
    addrWallet <- getWalletAddr
    case addressToKeyHashes addrWallet of
        Just hs -> pure hs
        Nothing -> throwM $ AddressDoesntCorrespondToPubKey addrWallet

genPrvKey :: HasWallet m => m XPrv
genPrvKey = do
    RestoredWallet{..} <- getRestoredWallet
    g <- getStdGen
    let (bs, _) = genByteString 2048 g
        pp = Ledger.Passphrase $ BS.pack $ BA.unpack $ Caradano.unPassphrase passphrase
    pure $ generateFromSeed bs pp

genPubKey :: HasWallet m => m PubKey
genPubKey = toPublicKey <$> genPrvKey

-- Get all value at a wallet
getWalletValue ::  (HasWallet m, HasChainIndexProvider m) => m P.Value
getWalletValue = mconcat . fmap decoratedTxOutPlutusValue . Map.elems <$> getWalletUtxos mempty

-- Get all ada at a wallet
getWalletAda :: (HasWallet m, HasChainIndexProvider m) => m P.Ada
getWalletAda = Ada.fromValue <$> getWalletValue

getWalletRefs :: (HasWallet m, HasChainIndexProvider m) => m [TxOutRef]
getWalletRefs = getWalletAddresses <&> NonEmpty.toList >>= concatMapM getRefsAt

-- Get all utxos at a wallet
getWalletUtxos :: (HasWallet m, HasChainIndexProvider m) => UtxoRequirements -> m MapUTXO
getWalletUtxos reqs = getWalletAddresses <&> NonEmpty.toList >>= mapM (getUtxosAt reqs) <&> mconcat

mkSignature :: HasWallet m => PV1.TxId -> m Signature
mkSignature txId = do
    RestoredWallet{..} <- getRestoredWallet
    xPrv <- genPrvKey
    let pp = Ledger.Passphrase $ BS.pack $ BA.unpack $ Caradano.unPassphrase passphrase
    pure $ signTx txId xPrv pp
