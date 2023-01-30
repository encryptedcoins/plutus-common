{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Utils.Address where

import           Data.Either.Extra               (eitherToMaybe)
import           Data.Text                       (Text)
import           Cardano.Api.Shelley             (AsType(..), StakeAddress(..), ShelleyEra, SerialiseAddress(..),
                                                    shelleyAddressInEra, byronAddressInEra, NetworkId)
import           Cardano.Ledger.Alonzo.TxInfo    (transKeyHash)
import qualified Cardano.Ledger.Credential       as Shelley

import           Control.Applicative             ((<|>))
import           Ledger                          (StakingCredential, toPlutusAddress, PubKeyHash (..))
import           Ledger.Address                  (StakePubKeyHash(..), Address(..), toPubKeyHash, stakingCredential)
import           Ledger.Tx.CardanoAPI            (toCardanoAddressInEra)

---------------------------- Address to keyhashes conversions ----------------------------------

addressToKeyHashes :: Address -> Maybe (PubKeyHash, Maybe StakingCredential)
addressToKeyHashes addr = do
    pkh  <- toPubKeyHash addr
    pure (pkh, stakingCredential addr)

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

-- Convert Plutus Address to bech32 text
addressToBech32 :: NetworkId -> Address -> Maybe Text
addressToBech32 networdId = fmap serialiseAddress . eitherToMaybe . toCardanoAddressInEra networdId

-- Convert bech32 Stake address to a Plutus StakePubKeyHash.
bech32ToStakePubKeyHash :: Text -> Maybe StakePubKeyHash
bech32ToStakePubKeyHash txt = do
    StakeAddress _ payCred <- deserialiseAddress AsStakeAddress txt
    case payCred of
            Shelley.ScriptHashObj _  -> Nothing
            Shelley.KeyHashObj kHash -> Just $ StakePubKeyHash $ transKeyHash kHash