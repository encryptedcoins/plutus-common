{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.IO.Wallet.Internal where

import qualified Cardano.Address               as Address
import           Cardano.Address.Derivation    (Depth (..), GenMasterKey (..), HardDerivation (..), XPrv, indexFromWord32, toXPub)
import           Cardano.Address.Style.Shelley (Credential (..))
import qualified Cardano.Address.Style.Shelley as S
import           Cardano.Api                   (NetworkId (..), StakeAddress)
import           Cardano.Mnemonic              (MkSomeMnemonic (..), SomeMnemonic)
import           Control.Exception             (Exception)
import           Control.FromSum               (maybeToEither)
import           Control.Lens                  ((<&>))
import           Control.Monad.Catch           (MonadThrow (..))
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Aeson                    (FromJSON (..), eitherDecode, withObject, (.:))
import           Data.Bifunctor                (Bifunctor (..))
import qualified Data.ByteString.Lazy          as LB
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.String                   (IsString (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Word                     (Word32)
import           GHC.Generics                  (Generic)
import           Ledger                        (Address (..), Passphrase (..), PubKeyHash)
import           PlutusAppsExtra.Types.Error   (WalletError (..))
import           PlutusAppsExtra.Utils.Address (addressToBech32, bech32ToAddress, getStakeKey, stakeAddressToBech32, stakeKeyToStakeAddress)
import           PlutusAppsExtra.Utils.Network (HasNetworkId (..))

------------------------------------------- Hi-level API -------------------------------------------

-- getWalletAddr :: HasWallet m => m Address
-- getWalletAddr = do
--     addressBech32 <- getWalletAddrBech32
--     maybe (throwM $ UnparsableWalletBech32 addressBech32) pure $ bech32ToAddress addressBech32

-- getWalletAddrBech32 :: HasWallet m => m Text
-- getWalletAddrBech32 = walletKeysToAddressBech32 <$> getWalletKeys <*> getNetworkId

getWalletStakeAddress :: HasWallet m => m StakeAddress
getWalletStakeAddress = do
    addr <- getWalletAddr
    networkId <- getNetworkId
    case getStakeKey addr of
        Just pkh -> maybe (throwM $ CantGetStakeAddressFromAddress addr) pure $ stakeKeyToStakeAddress networkId pkh
        _        -> throwM $ AddressWithoutStakingCredential addr

getWalletStakeAddressBech32 :: HasWallet m => m Text
getWalletStakeAddressBech32 = stakeAddressToBech32 <$> getWalletStakeAddress

getWalletPubKeyHash :: HasWallet m => m PubKeyHash
getWalletPubKeyHash = do
    addr <- getWalletAddr
    case getStakeKey addr of
        Just pkh -> pure pkh
        _        -> throwM $ AddressWithoutStakingCredential addr

getWalletKeys :: HasWallet m => m WalletKeys
getWalletKeys = either throwM pure . walletKeysFromMnemonic =<< getMnemonic

-- getWalletId :: HasWallet m => m WalletId
-- getWalletId = do
--     RestoredWallet{..} <- getRestoredWallet
--     pure $ genWalletId mnemonicSentence passphrase

data InternalWalletError
    = CantDeriveWalletKeys String
    | CantGetStakeAddressFromAddress Address
    | AddressWithoutStakingCredential Address
    | UnparsableWalletBech32 Text
    | Bech32EncodingError Address
    | NoAssociatedAddresses
    deriving (Show, Exception)

------------------------------------------- Restore-wallet -------------------------------------------

class (Monad m, MonadIO m, MonadThrow m, HasNetworkId m) => HasWallet m where
    getRestoredWallet  :: m RestoredWallet
    getWalletAddresses :: m (NonEmpty Address)

getWalletAddr :: HasWallet m => m Address
getWalletAddr = NonEmpty.head <$> getWalletAddresses

-- getWalletCardanoAddress :: HasWallet m => m Address.Address
-- getWalletCardanoAddress = do
--     addr <- getWalletAddr
--     networkId <- getNetworkId
--     let pkh = toPubKeyHash addr
--         sh = toScriptHash addr
--         z = makeShelleyAddress (toCardanoPaymentCredential _) _
--         x = serialiseAddress <$> toCardanoAddressInEra networkId addr
--     pure _ 
--     -- Address _ _ -> _

getWalletAddrBech32 :: HasWallet m => m Text
getWalletAddrBech32 = do
    networkId <- getNetworkId
    addr <- getWalletAddr
    maybe (throwM $ Bech32EncodingError addr) pure $ addressToBech32 networkId addr

data RestoredWallet = RestoredWallet
    { name             :: Text
    , mnemonicSentence :: SomeMnemonic
    , passphrase       :: Passphrase
    } deriving (Show, Generic)

instance FromJSON RestoredWallet where
    parseJSON = withObject "Restore wallet" $ \v -> do
        let mkMnemonic = either (fail . show) pure . mkSomeMnemonic @'[ 24 ]
            mkPassphrase = Passphrase . fromString
        name                   <- v .: "name"
        mnemonicSentence       <- v .: "mnemonic_sentence" >>= mkMnemonic
        passphrase             <- v .: "passphrase"        <&> mkPassphrase
        pure RestoredWallet{..}

getPassphrase :: HasWallet m => m Passphrase
getPassphrase = passphrase <$> getRestoredWallet

getMnemonic :: HasWallet m => m SomeMnemonic
getMnemonic = mnemonicSentence <$> getRestoredWallet

-- genWalletId :: SomeMnemonic -> Passphrase "user" -> WalletId
-- genWalletId mnemonic pp = WalletId $ digest $ publicKey rootXPrv
--   where
--     rootXPrv = generateKeyFromSeed (mnemonic, Nothing) pwdP
--     pwdP = preparePassphrase currentPassphraseScheme pp

addressFromMnemonic :: MonadThrow m => NetworkId -> SomeMnemonic -> m Address
addressFromMnemonic networkId mnemonic = do
    keys <- either throwM pure $ walletKeysFromMnemonic mnemonic
    let addrTxt = walletKeysToAddressBech32 keys networkId
    maybe (throwM $ UnparsableAddress addrTxt) pure $ bech32ToAddress addrTxt

restoreWalletFromFile :: (MonadIO m, MonadThrow m) => FilePath -> m RestoredWallet
restoreWalletFromFile fp = liftIO (LB.readFile fp) >>=
    either (throwM . RestoredWalletParsingError . T.pack) pure . eitherDecode

-- Read restore-wallet JSON file and generate walletId from it
-- walletIdFromFile :: (MonadIO m, MonadThrow m) => FilePath -> m WalletId
-- walletIdFromFile fp = do
--     RestoredWallet{..} <- restoreWalletFromFile fp
--     pure $ genWalletId mnemonicSentence passphrase

data WalletKeys = WalletKeys
  { wkRootKey    :: !(S.Shelley 'RootK XPrv)
  , wkAcctKey    :: !(S.Shelley 'AccountK XPrv)
  , wkPaymentKey :: !(S.Shelley 'PaymentK XPrv)
  , wkStakeKey   :: !(S.Shelley 'DelegationK XPrv)
  }

walletKeysFromMnemonicIndexed :: SomeMnemonic -> Word32 -> Word32 -> Either InternalWalletError WalletKeys
walletKeysFromMnemonicIndexed mnemonic nAcctIndex nAddrIndex = do
    accIx  <- maybeToEither invalidAccIndexErr $ indexFromWord32 $ minHardenedPathValue + nAcctIndex
    addrIx <- maybeToEither invalidAddrIndexErr$ indexFromWord32 nAddrIndex
    let wkRootKey    = genMasterKeyFromMnemonic mnemonic mempty
        wkAcctKey    = deriveAccountPrivateKey wkRootKey accIx
        wkPaymentKey = deriveAddressPrivateKey wkAcctKey S.UTxOExternal addrIx
        wkStakeKey   = S.deriveDelegationPrivateKey wkAcctKey
    pure WalletKeys {..}
    where
        minHardenedPathValue = 0x80000000
        invalidAccIndexErr  = CantDeriveWalletKeys $ "Invalid Account Index: " <> show nAcctIndex
        invalidAddrIndexErr = CantDeriveWalletKeys $ "Invalid Address Index: " <> show nAddrIndex

walletKeysFromMnemonic :: SomeMnemonic -> Either InternalWalletError WalletKeys
walletKeysFromMnemonic mnemonic = walletKeysFromMnemonicIndexed mnemonic 0 0

networkIdToNetworkDiscriminant :: NetworkId -> Address.NetworkDiscriminant S.Shelley
networkIdToNetworkDiscriminant = \case
    Mainnet   -> S.shelleyMainnet
    Testnet _ -> S.shelleyTestnet

-- | Gives the delegation address made using extended payment and stake keys.
walletKeysToAddressBech32 :: WalletKeys -> NetworkId -> Text
walletKeysToAddressBech32 WalletKeys{..} network = Address.bech32 $ S.delegationAddress networkTag paymentCredential delegationCredential
    where
        networkTag = networkIdToNetworkDiscriminant network
        paymentCredential = PaymentFromKey $ toXPub <$> wkPaymentKey
        delegationCredential = DelegationFromKey $ toXPub <$> wkStakeKey

walletKeysToStakeAddressBech32 :: WalletKeys -> NetworkId -> Either WalletError Text
walletKeysToStakeAddressBech32 WalletKeys{..} network = bimap (UnbuildableStakeAddress . T.pack . show) Address.bech32
    $ S.stakeAddress networkTag delegationCredential
    where
        networkTag = networkIdToNetworkDiscriminant network
        delegationCredential = DelegationFromKey $ toXPub <$> wkStakeKey
