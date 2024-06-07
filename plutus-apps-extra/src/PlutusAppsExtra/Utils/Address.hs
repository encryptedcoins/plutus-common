{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module PlutusAppsExtra.Utils.Address where

import           Cardano.Api.Shelley        (AsType (..), NetworkId (..), SerialiseAddress (..), ShelleyBasedEra (ShelleyBasedEraConway),
                                             StakeAddress (..), StakeCredential (..), byronAddressInEra, makeStakeAddress,
                                             shelleyAddressInEra)
import           Data.Either.Extra          (eitherToMaybe)
import           Data.Text                  (Text)

import           Cardano.Api                (serialiseToBech32)
import           Cardano.Ledger.Plutus      (transKeyHash)
import qualified Cardano.Ledger.Shelley.API as Shelley
import           Control.Applicative        ((<|>))
import           Control.Arrow              ((>>>))
import           Control.Monad              (MonadPlus (..))
import           Data.Aeson                 (FromJSON (parseJSON))
import qualified Data.Aeson                 as J
import           Ledger                     (PubKeyHash (..), StakingCredential, toPlutusAddress)
import           Ledger.Address             (Address (..), StakePubKeyHash (..), stakingCredential, toPubKeyHash)
import           Ledger.Tx.CardanoAPI       (deserialiseFromRawBytes, toCardanoAddressInEra)
import           PlutusLedgerApi.V3         (Credential (..), StakingCredential (..), fromBuiltin)

---------------------------- Address to keyhashes conversions ----------------------------------

addressToKeyHashes :: Address -> Maybe (PubKeyHash, Maybe StakingCredential)
addressToKeyHashes addr = do
    pkh  <- toPubKeyHash addr
    pure (pkh, stakingCredential addr)

getStakeKey :: Address -> Maybe PubKeyHash
getStakeKey = stakingCredential >>> \case
    Just (StakingHash (PubKeyCredential pkh)) -> Just pkh
    _ -> Nothing

----------------------------------- Bech32 conversions -----------------------------------------

newtype Bech32Address = Bech32Address {unBech32Address :: Address}

instance FromJSON Bech32Address where
    parseJSON = J.withText "Bech32Address" $ \s -> maybe mzero (pure . Bech32Address) $ bech32ToAddress s

-- TODO: simplify address conversions using the new Plutus.Ledger functions

-- Extract key hashes from bech32 Shelley/Byron address
bech32ToKeyHashes :: Text -> Maybe (PubKeyHash, Maybe StakingCredential)
bech32ToKeyHashes txt = do
    addr <- bech32ToAddress txt
    addressToKeyHashes addr

-- Convert bech32 Shelley/Byron address to Plutus Address
bech32ToAddress :: Text -> Maybe Address
bech32ToAddress txt = toPlutusAddress <$> (shelleyAddr <|> byronAddr)
    where
        shelleyAddr = shelleyAddressInEra ShelleyBasedEraConway <$> deserialiseAddress AsShelleyAddress txt
        byronAddr = byronAddressInEra <$> deserialiseAddress AsByronAddress txt

bech32ToStakeAddress :: Text -> Maybe StakeAddress
bech32ToStakeAddress = deserialiseAddress AsStakeAddress

-- Convert Plutus Address to bech32 text
addressToBech32 :: NetworkId -> Address -> Maybe Text
addressToBech32 networkId = fmap serialiseAddress . eitherToMaybe . toCardanoAddressInEra networkId

stakeAddressToBech32 :: StakeAddress -> Text
stakeAddressToBech32 = serialiseToBech32

-- Convert bech32 Stake address to a Plutus StakePubKeyHash.
bech32ToStakePubKeyHash :: Text -> Maybe StakePubKeyHash
bech32ToStakePubKeyHash txt = do
    StakeAddress _ payCred <- bech32ToStakeAddress txt
    case payCred of
            Shelley.ScriptHashObj _  -> Nothing
            Shelley.KeyHashObj kHash -> Just $ StakePubKeyHash $ transKeyHash kHash

------------------------------------- Other conversions -------------------------------------------

spkhToStakeCredential :: StakePubKeyHash -> Maybe StakeCredential
spkhToStakeCredential (StakePubKeyHash (PubKeyHash bbs)) = fmap StakeCredentialByKey
    $ eitherToMaybe $ deserialiseFromRawBytes (AsHash AsStakeKey) $ fromBuiltin bbs

stakeKeyToStakeAddress :: NetworkId -> PubKeyHash -> Maybe StakeAddress
stakeKeyToStakeAddress networkId (PubKeyHash h) = eitherToMaybe $ makeStakeAddress networkId . StakeCredentialByKey
    <$> deserialiseFromRawBytes (AsHash AsStakeKey) (fromBuiltin h)
