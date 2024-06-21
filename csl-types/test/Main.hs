{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell           #-}

module Main where

import qualified Data.Aeson as Aeson
import           CSL.Class                         (FromCSL (fromCSL), ToCSL (toCSL))
import           Cardano.Api                       (NetworkId (Mainnet, Testnet), NetworkMagic (NetworkMagic), valueFromList,
                                                    valueToList)
import qualified Cardano.Api                       as C
import           Control.Monad.IO.Class            (MonadIO (..))
import           Data.Bifunctor                    (Bifunctor (..))
import           Data.ByteString                   (ByteString)
import           Data.Maybe                        (isJust)
import qualified Ledger
import           Ledger                            (policyId, Versioned (..), Language (..), Address (Address), DecoratedTxOut (..), PubKeyHash, StakingCredential, TxOutRef,
                                                    Value)
import           Ledger.Value.CardanoAPI           (fromCardanoValue, toCardanoValue)
import           PlutusAppsExtra.Utils.Address     (addressToBech32)
import           PlutusLedgerApi.V1                (Credential (..))
import           PlutusLedgerApi.V2                (getValue)
import qualified PlutusLedgerApi.V2                as P
import qualified PlutusTx
import qualified PlutusTx.AssocMap                 as PAM
import           Test.Hspec                        (Expectation, Spec, describe, hspec, it, shouldBe)
import           Test.QuickCheck                   (Arbitrary (..), Gen, Testable (property), generate, listOf, oneof)
import           Test.QuickCheck.Arbitrary.Generic

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "toCSL . fromCSL == id" $ do
    it "TxOutRef" $ property propTxOutRef
    it "[TxOutRef]" $ property propTxOutRefs
    it "Value" $ property propValue
    it "DecoratedTxOut" $ property propDecoratedTxOut
    it "[DecoratedTxOut]" $ property propDecoratedTxOuts
    it "(TxOutRef, DecoratedTxOut)" $ property propRefWithDecoratedTxOut
    it "[TxOutRef, DecoratedTxOut]" $ property propRefsWithDecoratedTxOuts

propTxOutRef :: TxOutRef -> Expectation
propTxOutRef ref = (toCSL ref >>= fromCSL) `shouldBe` Just ref

propTxOutRefs :: [TxOutRef] -> Expectation
propTxOutRefs refs = (toCSL refs >>= fromCSL) `shouldBe` Just refs

propValue :: Value' -> Expectation
propValue (Value' val) = (toCSL (fromCardanoValue val) >>= fromCSL) `shouldBe` Just (fromCardanoValue val)

propDecoratedTxOut :: () -> Expectation
propDecoratedTxOut _ = do
    (PublicKeyDecoratedTxOut' txOut, NetworkId' network) <- liftIO $ generate arbitrary
    (toCSL (txOut, network) >>= fromCSL) `shouldBe` Just txOut

propDecoratedTxOuts :: () -> Expectation
propDecoratedTxOuts _ = do
    (txOuts, NetworkId' network) <- fmap (first (fmap unPKDO')) $ liftIO $ generate $ arbitrary @([PublicKeyDecoratedTxOut'], NetworkId')
    (toCSL (txOuts, network) >>= fromCSL) `shouldBe` Just txOuts

propRefWithDecoratedTxOut :: TxOutRef -> Expectation
propRefWithDecoratedTxOut ref = do
    (PublicKeyDecoratedTxOut' txOut, NetworkId' network) <- liftIO $ generate arbitrary
    (toCSL (ref, txOut, network) >>= fromCSL) `shouldBe` Just (ref, txOut)

propRefsWithDecoratedTxOuts :: [TxOutRef] -> Expectation
propRefsWithDecoratedTxOuts refs = do
    (txOuts, NetworkId' network) <- fmap (first (fmap unPKDO')) $ liftIO $ generate arbitrary
    let xs = zip refs txOuts
    (toCSL (xs, network) >>= fromCSL) `shouldBe` Just xs

newtype Value' = Value' C.Value
    deriving newtype Show

instance Arbitrary Value' where
    arbitrary = do
        val <- arbitrary
        if validValue val then pure $ Value' val else arbitrary
        where validValue val = fromCardanoValue val == (P.Value . PAM.unsafeFromList . PAM.toList $ getValue $ fromCardanoValue val)

newtype PublicKeyDecoratedTxOut' = PublicKeyDecoratedTxOut' {unPKDO' :: DecoratedTxOut}
    deriving newtype Show

instance {-# OVERLAPPING #-} Arbitrary (PublicKeyDecoratedTxOut', NetworkId') where
    arbitrary = do
        (NetworkId' network) <- arbitrary
        pkh <- arbitrary
        sc  <- arbitrary
        if not $ validAddr network pkh sc then arbitrary else do
            Value' val <- arbitrary
            pure $ (,NetworkId' network) $ PublicKeyDecoratedTxOut' $ PublicKeyDecoratedTxOut pkh sc val Nothing Nothing
        where validAddr network pkh sc = isJust $ addressToBech32 network $ Address (PubKeyCredential pkh) sc

instance {-# OVERLAPPING #-} Arbitrary ([PublicKeyDecoratedTxOut'], NetworkId') where
    arbitrary = do
        res <- unzip <$> listOf arbitrary
        pure $ head <$> res

newtype NetworkId' = NetworkId' NetworkId
    deriving newtype Show

instance Arbitrary NetworkId' where
    arbitrary = fmap NetworkId' $ oneof $ map pure [Mainnet, Testnet $ NetworkMagic 1, Testnet $ NetworkMagic 2]

-- instance Arbitrary Aeson.Value where
--     arbitrary = oneof [Aeson.String <$> arbitrary, Aeson.Number <$> arbitrary]

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

instance Arbitrary StakingCredential where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Credential where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PubKeyHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary P.ScriptHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxOutRef where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary P.TxId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary P.BuiltinByteString where
    arbitrary = P.toBuiltin <$> (arbitrary :: Gen ByteString)

-- | A minting policy that always succeeds.
acceptingMintingPolicy :: Ledger.MintingPolicy
acceptingMintingPolicy = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [|| (\_ _ -> ()) ||])