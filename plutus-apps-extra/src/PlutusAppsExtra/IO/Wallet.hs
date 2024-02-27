{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.IO.Wallet where

import           Cardano.Address.Derivation             (XPrv)
import qualified Cardano.Wallet.Primitive.Passphrase    as Caradano
import           Cardano.Wallet.Primitive.Types.Address (AddressState (..))
import           Control.Lens                           ((<&>))
import           Control.Monad.Catch                    (MonadThrow (..))
import           Control.Monad.Extra                    (concatMapM)
import qualified Data.ByteArray                         as BA
import qualified Data.ByteString                        as BS
import           Data.List.NonEmpty                     (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                     as NonEmpty
import qualified Data.Map                               as Map
import           Ledger                                 (Address, Passphrase (..), PubKey, PubKeyHash, Signature, StakingCredential, TxId,
                                                         TxOutRef, decoratedTxOutPlutusValue, generateFromSeed, toPublicKey)
import           Ledger.Crypto                          (signTx)
import qualified Plutus.Script.Utils.Ada                as Ada
import qualified Plutus.Script.Utils.Ada                as P
import qualified Plutus.V2.Ledger.Api                   as P
import           PlutusAppsExtra.IO.ChainIndex          (HasChainIndexProvider, getRefsAt, getUtxosAt)
import qualified PlutusAppsExtra.IO.Wallet.Cardano      as Cardano
import           PlutusAppsExtra.IO.Wallet.Internal     (HasWallet (getRestoredWallet), RestoredWallet (..))
import           PlutusAppsExtra.Types.Error            (WalletError (..))
import           PlutusAppsExtra.Types.Tx               (UtxoRequirements)
import           PlutusAppsExtra.Utils.Address          (addressToKeyHashes)
import           PlutusAppsExtra.Utils.ChainIndex       (MapUTXO)
import           Prelude                                hiding ((-))
import           System.Random                          (genByteString, getStdGen)

data WalletProvider = Cardano | Lightweight (NonEmpty Address)
    deriving (Show, Eq)

class (HasWallet m) => HasWalletProvider m where

    getWalletProvider :: m WalletProvider

    getWalletAddr :: m Address
    getWalletAddr = getWalletProvider >>= \case
        Cardano                 -> Cardano.getWalletAddr
        Lightweight (addr :| _) -> pure addr

    getWalletAddresses :: m [Address]
    getWalletAddresses = getWalletProvider >>= \case
        Cardano           -> Cardano.ownAddresses (Just Used)
        Lightweight addrs -> pure $ NonEmpty.toList addrs

getWalletKeyHashes :: HasWalletProvider m => m (PubKeyHash, Maybe StakingCredential)
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
getWalletValue ::  (HasWalletProvider m, HasChainIndexProvider m) => m P.Value
getWalletValue = mconcat . fmap decoratedTxOutPlutusValue . Map.elems <$> getWalletUtxos mempty

-- Get all ada at a wallet
getWalletAda :: (HasWalletProvider m, HasChainIndexProvider m) => m P.Ada
getWalletAda = Ada.fromValue <$> getWalletValue

getWalletRefs :: (HasWalletProvider m, HasChainIndexProvider m) => m [TxOutRef]
getWalletRefs = getWalletAddresses >>= concatMapM getRefsAt

-- Get all utxos at a wallet
getWalletUtxos :: (HasWalletProvider m, HasChainIndexProvider m) => UtxoRequirements -> m MapUTXO
getWalletUtxos reqs = getWalletAddresses >>= mapM (getUtxosAt reqs) <&> mconcat

mkSignature :: HasWallet m => TxId -> m Signature
mkSignature txId = do
    RestoredWallet{..} <- getRestoredWallet
    xPrv <- genPrvKey
    let pp = Ledger.Passphrase $ BS.pack $ BA.unpack $ Caradano.unPassphrase passphrase
    pure $ signTx txId xPrv pp