{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Wallet where

import           Cardano.Api.Shelley                 (NetworkId (..), NetworkMagic (..), ProtocolParameters (..))
import           Cardano.Mnemonic                    (MkSomeMnemonic (..))
import           Cardano.Node.Emulator.Params        (Params (..))
import           Cardano.Wallet.Primitive.Passphrase (Passphrase (..))
import           Cardano.Wallet.Primitive.Types      (WalletId (..))
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (decode)
import           Data.ByteString.Lazy                (readFile)
import           Data.Default                        (Default (..))
import           Data.Either                         (fromRight)
import           Data.Maybe                          (fromJust)
import           Data.String                         (IsString (fromString))
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Ledger                              (TxOutRef, stakingCredential)
import           PlutusAppsExtra.IO.ChainIndex       (ChainIndexProvider (..), HasChainIndexProvider (..))
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo  as Kupo
import           PlutusAppsExtra.IO.Tx               (HasTxProvider (..), getWalletTxOutRefs)
import qualified PlutusAppsExtra.IO.Tx               as Tx
import qualified PlutusAppsExtra.IO.Tx.Cardano       as Cardano
import           PlutusAppsExtra.IO.Wallet           (HasWalletProvider (..))
import qualified PlutusAppsExtra.IO.Wallet           as Wallet
import           PlutusAppsExtra.IO.Wallet.Internal  (HasWallet (..), genWalletId, restoreWalletFromFile, walletIdFromFile)
import           PlutusAppsExtra.Utils.Address       (bech32ToAddress, bech32ToKeyHashes)
import           PlutusAppsExtra.Utils.Network       (HasNetworkId (..))
import           Prelude                             hiding (readFile)

instance HasNetworkId IO where
    getNetworkId = pure $ Testnet (NetworkMagic 1)

instance HasWallet IO where
    getRestoredWallet = restoreWalletFromFile "testnet/wallet.json"

instance HasWalletProvider IO where
    getWalletProvider = pure Wallet.Cardano

instance HasChainIndexProvider IO where
    getChainIndexProvider = pure Kupo
    getUtxosAt = (liftIO .) . Kupo.getUtxosAt
    getUnspentTxOutFromRef = (liftIO .) . Kupo.getUnspentTxOutFromRef

instance HasTxProvider IO where
    getTxProvider = pure $ Tx.Cardano ""
    submitTx = Cardano.submitTx
    awaitTxConfirmed = Cardano.awaitTxConfirmed


daedalusAddress :: Text
daedalusAddress = "addr_test1qpmv0wkr6z9sdqveecpuywrwcxyueft0wgle85cs9fhsvtgnt9a4spnfrrlpp7puw2lcx2zudf49ewyza4q9ha08qhdq7aezrw"

test :: FilePath -> IO [TxOutRef]
test file = do
  pp <- fromJust . decode <$> readFile file
  let pkh = fromJust $ fst <$> bech32ToKeyHashes daedalusAddress
      sc = stakingCredential $ fromJust $ bech32ToAddress daedalusAddress
      networkId = Testnet $ NetworkMagic 1097911063
      ledgerParams = Params def (emulatorPParams pp) networkId
  getWalletTxOutRefs ledgerParams pkh sc 1

testWalletGen :: Bool
testWalletGen = "2233e2fc50a0d880e187f4a87de74234e960bd95" == show
    (genWalletId
    (fromRight undefined $ mkSomeMnemonic @'[ 24 ] . T.words $
    "chronic distance live brand switch angry erase empty lobster defy disorder flush moon burst acoustic miracle creek wild excuse spike fork neutral market shuffle")
    (Passphrase $ fromString "1234567890"))

testWaletIdFromFile :: FilePath -> IO WalletId
testWaletIdFromFile = walletIdFromFile