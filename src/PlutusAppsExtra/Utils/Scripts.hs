{-# LANGUAGE FlexibleContexts #-}

module PlutusAppsExtra.Utils.Scripts where

import qualified Codec.CBOR.Decoding   as CBOR
import           Codec.Serialise       (Serialise (encode), decode, deserialise, serialise)
import qualified Data.ByteString.Lazy  as LBS
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import           Data.Coerce           (coerce)
import qualified Flat
import qualified Flat.Decoder          as Flat
import           Ledger                (MintingPolicy (..), Script (..), Validator (..), mkMintingPolicyScript, mkValidatorScript)
import           PlutusCore            (DefaultFun, DefaultUni)
import qualified PlutusTx
import           PlutusTx.Code         (CompiledCode)
import           Text.Hex              (Text, decodeHex, encodeHex)
import qualified UntypedPlutusCore     as UPLC

validatorToCBOR :: Validator -> Text
validatorToCBOR = encodeHex . SBS.fromShort . serialiseUPLC . coerce

validatorFromCBOR :: Text -> Maybe Validator
validatorFromCBOR = fmap (coerce . deserialiseUPLC . SBS.toShort) . decodeHex

parametrizedValidatorFromCBOR :: PlutusTx.Lift DefaultUni a => Text -> Maybe (a -> Validator)
parametrizedValidatorFromCBOR txt = fmap mkValidatorScript <$> parametrizedCompiledCodeFromCBOR txt

mintingPolicyToCBOR :: MintingPolicy -> Text
mintingPolicyToCBOR = encodeHex . SBS.fromShort . serialiseUPLC . coerce

mintingPolicyFromCBOR :: Text -> Maybe MintingPolicy
mintingPolicyFromCBOR = fmap (coerce . deserialiseUPLC . SBS.toShort) . decodeHex

parametrizedMintingPolicyFromCBOR :: PlutusTx.Lift DefaultUni a => Text -> Maybe (a -> MintingPolicy)
parametrizedMintingPolicyFromCBOR txt = fmap mkMintingPolicyScript <$> parametrizedCompiledCodeFromCBOR txt

type SerialisedScript = ShortByteString

parametrizedCompiledCodeFromCBOR :: PlutusTx.Lift DefaultUni par => Text -> Maybe (par -> PlutusTx.CompiledCodeIn DefaultUni DefaultFun x)
parametrizedCompiledCodeFromCBOR txt = do
    bs <- decodeHex txt
    let code = deserialiseCompiledCode $ SBS.toShort bs
    pure $ \a -> code `PlutusTx.applyCode` PlutusTx.liftCode a

serialiseUPLC :: UPLC.Program UPLC.DeBruijn DefaultUni DefaultFun () -> SerialisedScript
serialiseUPLC =
    -- See Note [Using Flat for serialising/deserialising Script]
    -- Currently, this is off because the old implementation didn't actually work, so we need to be careful
    -- about introducing a working versioPlutusLedgerApi.Common.SerialisedScriptn
    SBS.toShort . LBS.toStrict . serialise . SerialiseViaFlat

-- | Deserialises a 'SerialisedScript' back into an AST.
deserialiseUPLC :: SerialisedScript -> UPLC.Program UPLC.DeBruijn DefaultUni DefaultFun ()
deserialiseUPLC = unSerialiseViaFlat . deserialise . LBS.fromStrict . SBS.fromShort
    where
        unSerialiseViaFlat (SerialiseViaFlat a) = a

deserialiseCompiledCode :: SerialisedScript -> CompiledCode a
deserialiseCompiledCode = unSerialiseViaFlat . deserialise . LBS.fromStrict . SBS.fromShort
     where
        unSerialiseViaFlat (SerialiseViaFlat a) = a

-- | Newtype to provide 'Serialise' instances for types with a 'Flat' instance that
-- just encodes the flat-serialized value as a CBOR bytestring
newtype SerialiseViaFlat a = SerialiseViaFlat a
instance Flat.Flat a => Serialise (SerialiseViaFlat a) where
    encode (SerialiseViaFlat a) = encode $ Flat.flat a
    decode = SerialiseViaFlat <$> decodeViaFlat Flat.decode

decodeViaFlat :: Flat.Get a -> CBOR.Decoder s a
decodeViaFlat decoder = do
    bs <- CBOR.decodeBytes
    -- lift any flat's failures to be cborg failures (MonadFail)
    either (fail . show) pure $
        Flat.unflatWith decoder bs