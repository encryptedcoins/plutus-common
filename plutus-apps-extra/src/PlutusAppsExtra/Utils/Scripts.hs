{-# LANGUAGE FlexibleContexts #-}

module PlutusAppsExtra.Utils.Scripts where

import qualified Codec.CBOR.Decoding   as CBOR
import           Codec.Serialise       (Serialise (encode), decode, deserialise, serialise)
import qualified Data.ByteString.Lazy  as LBS
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import           Data.Coerce           (coerce, Coercible)
import           Data.Maybe            (fromJust)
import qualified Flat
import qualified Flat.Decoder          as Flat
import           Ledger                (MintingPolicy (..), Script (..), Validator (..))
import           Plutus.V2.Ledger.Api  (ToData, toData)
import           PlutusCore            (DefaultFun, DefaultUni, defaultVersion)
import           PlutusCore.MkPlc      (mkConstant)
import           Text.Hex              (Text, decodeHex, encodeHex)
import           UntypedPlutusCore     (Program (..), DeBruijn, applyProgram)

type UPLCProgram = Program DeBruijn DefaultUni DefaultFun ()

----------------------- To CBOR -----------------------

scriptToCBOR :: Coercible script UPLCProgram => script -> Text
scriptToCBOR = encodeHex . SBS.fromShort . serialiseUPLC . coerce

validatorToCBOR :: Validator -> Text
validatorToCBOR = scriptToCBOR

mintingPolicyToCBOR :: MintingPolicy -> Text
mintingPolicyToCBOR = scriptToCBOR

----------------------- From CBOR -----------------------

scriptFromCBOR :: Coercible UPLCProgram script => Text -> Maybe script
scriptFromCBOR = fmap (coerce . deserialiseUPLC . SBS.toShort) . decodeHex

validatorFromCBOR :: Text -> Maybe Validator
validatorFromCBOR = scriptFromCBOR

unsafeValidatorFromCBOR :: Text -> Validator
unsafeValidatorFromCBOR = fromJust . validatorFromCBOR

mintingPolicyFromCBOR :: Text -> Maybe MintingPolicy
mintingPolicyFromCBOR = scriptFromCBOR

unsafeMintingPolicyFromCBOR :: Text -> MintingPolicy
unsafeMintingPolicyFromCBOR = fromJust . mintingPolicyFromCBOR

--------------- Parameterized from CBOR ------------------

parameterizedScriptFromCBOR :: (ToData par, Coercible UPLCProgram script)
    => Text -> Maybe (par -> script)
parameterizedScriptFromCBOR txt = do
    bs <- decodeHex txt
    let program = deserialiseUPLC $ SBS.toShort bs
        p = Program () (defaultVersion ()) . mkConstant ()
    pure $ \a -> coerce $ program `applyProgram` p (toData a)

parameterizedValidatorFromCBOR :: (ToData a) => Text -> Maybe (a -> Validator)
parameterizedValidatorFromCBOR = parameterizedScriptFromCBOR

unsafeParameterizedValidatorFromCBOR :: (ToData a) => Text -> a -> Validator
unsafeParameterizedValidatorFromCBOR = fromJust . parameterizedValidatorFromCBOR

parameterizedMintingPolicyFromCBOR :: (ToData a) => Text -> Maybe (a -> MintingPolicy)
parameterizedMintingPolicyFromCBOR = parameterizedScriptFromCBOR

unsafeParameterizedMintingPolicyFromCBOR :: (ToData a) => Text -> a -> MintingPolicy
unsafeParameterizedMintingPolicyFromCBOR = fromJust . parameterizedMintingPolicyFromCBOR

----------------------- Serialisation -----------------------

type SerialisedScript = ShortByteString

serialiseUPLC :: Program DeBruijn DefaultUni DefaultFun () -> SerialisedScript
serialiseUPLC =
    -- See Note [Using Flat for serialising/deserialising Script]
    -- Currently, this is off because the old implementation didn't actually work, so we need to be careful
    -- about introducing a working versioPlutusLedgerApi.Common.SerialisedScriptn
    SBS.toShort . LBS.toStrict . serialise . SerialiseViaFlat

-- | Deserialises a 'SerialisedScript' back into an AST.
deserialiseUPLC :: SerialisedScript -> Program DeBruijn DefaultUni DefaultFun ()
deserialiseUPLC = unSerialiseViaFlat . deserialise . LBS.fromStrict . SBS.fromShort
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