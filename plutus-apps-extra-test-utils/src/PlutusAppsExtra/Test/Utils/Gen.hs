{-# LANGUAGE DataKinds                           #-}
{-# LANGUAGE FlexibleInstances                   #-}
{-# LANGUAGE TemplateHaskell                     #-}
{-# OPTIONS_GHC -Wno-orphans                     #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

module PlutusAppsExtra.Test.Utils.Gen where

import qualified Cardano.Api                       as C
import           Cardano.Node.Emulator             ()
import qualified Data.ByteString                   as BS
import           Control.Monad                     (replicateM)
import           Data.ByteString                   (ByteString)
import           Data.Either.Combinators           (rightToMaybe)
import           Ledger                            (Address (..), PaymentPubKey (..), PubKey (..), PubKeyHash (..), TxOutRef (..), TxOut (TxOut), ValidatorHash (..), pubKeyAddress)
import qualified Ledger
import           Ledger.Address                    (PaymentPubKeyHash, StakePubKey, StakePubKeyHash)
import           Ledger.Crypto                     (Signature)
import           Ledger.Scripts                    (Language (..), Versioned (..))
import           Ledger.Slot                       (Slot)
import           Ledger.Tx                         (Certificate, RedeemerPtr, ScriptTag, TxId, TxIn, Withdrawal)
import           Ledger.Tx.CardanoAPI              (ToCardanoError, toCardanoAddressInEra, toCardanoTxOut)
import           Ledger.Value.CardanoAPI           (policyId)
import qualified Plutus.Script.Utils.Ada           as Plutus
import           Plutus.Script.Utils.V1.Address    (mkValidatorAddress)
import qualified Plutus.Script.Utils.Value         as Plutus
import qualified PlutusLedgerApi.V1.Tx             as PV1
import           PlutusLedgerApi.V1                (LedgerBytes)
import qualified PlutusLedgerApi.V2                as PV2
import           PlutusLedgerApi.V3                (Credential (..), StakingCredential (..), fromBytes, toBuiltin)
import qualified PlutusTx
import qualified PlutusTx.AssocMap                 as AssocMap
import qualified PlutusTx.Prelude                  as PlutusTx
import           Test.QuickCheck                   (Arbitrary (..), Gen, choose, oneof, suchThatMap)
import           Test.QuickCheck.Arbitrary.Generic (Arg, genericArbitrary, genericShrink)
import           Test.QuickCheck.Instances         ()

genPubKeyAddress :: Gen Address
genPubKeyAddress = do
    lb <- fromBytes <$> arbitrary
    pure $ pubKeyAddress (PaymentPubKey (PubKey lb)) Nothing

genPubKeyAddressWithStakingHash :: Gen Address
genPubKeyAddressWithStakingHash = do
    Address cred _ <- genPubKeyAddress
    pkh <- PubKeyHash . toBuiltin . BS.pack <$> replicateM 28 arbitrary
    pure $ Address cred $ Just (StakingHash (PubKeyCredential pkh))

-- An existing arbitrary instance is rejected by a local node
genTxOutRef :: Gen TxOutRef
genTxOutRef = TxOutRef
    <$> (PV1.TxId . toBuiltin . BS.pack <$> replicateM 32 arbitrary)
    <*> choose (1,40)

genCollateral :: Gen (Maybe TxOutRef)
genCollateral = arbitrary >>= \b -> if b then pure Nothing else Just <$> genTxOutRef

-- | A validator that always succeeds.
acceptingValidator :: Ledger.Validator
acceptingValidator = Ledger.mkValidatorScript $$(PlutusTx.compile [|| (\_ _ _ -> ()) ||])

-- | A minting policy that always succeeds.
acceptingMintingPolicy :: Ledger.MintingPolicy
acceptingMintingPolicy = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [|| (\_ _ -> ()) ||])

instance Arbitrary PlutusTx.BuiltinByteString where
    arbitrary = PlutusTx.toBuiltin <$> (arbitrary :: Gen ByteString)

instance Arbitrary LedgerBytes where
    arbitrary = fromBytes <$> arbitrary

instance Arbitrary Ledger.MintingPolicy where
    arbitrary = pure acceptingMintingPolicy

instance Arbitrary Ledger.MintingPolicyHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Script where
    arbitrary = oneof [
          pure $ Ledger.unValidatorScript acceptingValidator
        , pure $ Ledger.unMintingPolicyScript acceptingMintingPolicy
        ]

instance Arbitrary Ledger.ScriptHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.ValidationError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.ScriptError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ToCardanoError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxIn where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.TxIx where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PV2.OutputDatum where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxOut where
    arbitrary = fmap (fmap TxOut . toCardanoTxOut (C.Testnet (C.NetworkMagic 1))) genericArbitrary `suchThatMap` rightToMaybe
    shrink = pure

instance Arbitrary TxOutRef where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Withdrawal where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Certificate where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Credential where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.StakingCredential where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.DCert where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ScriptTag where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RedeemerPtr where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PubKey where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PubKeyHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PaymentPubKey where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PaymentPubKeyHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary StakePubKey where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary StakePubKeyHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Slot where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PV2.TxId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Signature where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PlutusTx.BuiltinData where
    arbitrary = PlutusTx.dataToBuiltinData <$> arbitrary
    shrink d = PlutusTx.dataToBuiltinData <$> shrink (PlutusTx.builtinDataToData d)

instance (Arg (Ledger.Versioned script) script, Arbitrary script) => Arbitrary (Ledger.Versioned script) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Datum where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.DatumHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Redeemer where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Validator where
    arbitrary = pure acceptingValidator

instance Arbitrary Plutus.TokenName where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Plutus.CurrencySymbol where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Plutus.Ada where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Plutus.Value where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary C.Value where
    arbitrary = C.valueFromList <$> arbitrary

instance Arbitrary C.AssetId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary C.AssetName where
    arbitrary = C.AssetName <$> arbitrary

instance Arbitrary C.Quantity where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary C.PolicyId where
    arbitrary = pure $ policyId (Versioned acceptingMintingPolicy PlutusV1)

instance (Arbitrary k, Arbitrary v) => Arbitrary (AssocMap.Map k v) where
    arbitrary = AssocMap.unsafeFromList <$> arbitrary

instance Arbitrary Address where
    arbitrary = oneof [Ledger.pubKeyAddress <$> arbitrary <*> arbitrary, mkValidatorAddress <$> arbitrary]

instance Arbitrary (C.AddressInEra C.ConwayEra) where
    arbitrary = fmap (toCardanoAddressInEra (C.Testnet (C.NetworkMagic 1))) genericArbitrary `suchThatMap` rightToMaybe

instance Arbitrary ValidatorHash where
    arbitrary = ValidatorHash <$> arbitrary