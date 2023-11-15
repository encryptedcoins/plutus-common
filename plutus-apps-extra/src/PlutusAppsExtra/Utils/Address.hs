{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module PlutusAppsExtra.Utils.Address where

import           Cardano.Api.Shelley          (AsType (..), NetworkId (..), SerialiseAddress (..), ShelleyEra, StakeAddress (..),
                                               StakeCredential (..), byronAddressInEra, shelleyAddressInEra, makeStakeAddress)
import           Cardano.Ledger.Alonzo.TxInfo (transKeyHash)
import qualified Cardano.Ledger.Credential    as Shelley
import           Data.Either.Extra            (eitherToMaybe)
import           Data.Text                    (Text)

import           Control.Applicative          ((<|>))
import           Control.Arrow                ((>>>))
import           Ledger                       (PubKeyHash (..), StakingCredential, toPlutusAddress)
import           Ledger.Address               (Address (..), StakePubKeyHash (..), stakingCredential, toPubKeyHash)
import           Ledger.Tx.CardanoAPI         (deserialiseFromRawBytes, toCardanoAddressInEra)
import           Plutus.V1.Ledger.Api         (Credential (..), StakingCredential (..), fromBuiltin)

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
        shelleyAddr = shelleyAddressInEra @ShelleyEra <$> deserialiseAddress AsShelleyAddress txt
        byronAddr = byronAddressInEra <$> deserialiseAddress AsByronAddress txt

bech32ToStakeAddress :: Text -> Maybe StakeAddress
bech32ToStakeAddress = deserialiseAddress AsStakeAddress

-- Convert Plutus Address to bech32 text
addressToBech32 :: NetworkId -> Address -> Maybe Text
addressToBech32 networkId = fmap serialiseAddress . eitherToMaybe . toCardanoAddressInEra networkId

-- Convert bech32 Stake address to a Plutus StakePubKeyHash.
bech32ToStakePubKeyHash :: Text -> Maybe StakePubKeyHash
bech32ToStakePubKeyHash txt = do
    StakeAddress _ payCred <- deserialiseAddress AsStakeAddress txt
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