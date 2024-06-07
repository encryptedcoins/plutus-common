{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module PlutusAppsExtra.Utils.Scripts where

-- import           Codec.Serialise      (deserialise, serialise)
-- import qualified Data.ByteString.Lazy as LBS
-- import           Data.Coerce          (Coercible, coerce)
-- import           Data.Maybe           (fromJust)
-- import           Ledger               (MintingPolicy (..), Script (..), Validator (..))
-- import           PlutusCore           (DefaultFun, DefaultUni)
-- import           Text.Hex             (Text, decodeHex, encodeHex)
-- import           UntypedPlutusCore    (DeBruijn, Program (..))
import qualified Codec.CBOR.Decoding       as CBOR
import           Codec.Serialise           (Serialise (encode), decode, deserialise, serialise)
import           Control.FromSum           (eitherToMaybe)
import           Control.Monad.Error.Class (MonadError)
import qualified Data.ByteString.Lazy      as LBS
import           Data.ByteString.Short     (ShortByteString)
import qualified Data.ByteString.Short     as SBS
import           Data.Coerce               (Coercible, coerce)
import           Data.Maybe                (fromJust)
import qualified Flat
import qualified Flat.Decoder              as Flat
import           Ledger                    (MintingPolicy (..), Script (..), Validator (..))
import           PlutusCore                (DefaultFun, DefaultUni, latestVersion)
import           PlutusCore.Error          (ApplyProgramError)
import           PlutusCore.MkPlc          (mkConstant)
import           PlutusLedgerApi.V3        (ToData, serialiseUPLC, toData, uncheckedDeserialiseUPLC)
import           Text.Hex                  (Text, decodeHex, encodeHex)
import           UntypedPlutusCore         (DeBruijn, Program (..), applyProgram)

type UPLCProgram = Program DeBruijn DefaultUni DefaultFun ()

----------------------- To CBOR -----------------------

scriptToCBOR :: Coercible Script script => script -> Text
scriptToCBOR = encodeHex . LBS.toStrict . serialise . coerce @_ @Script

validatorToCBOR :: Validator -> Text
validatorToCBOR = scriptToCBOR

mintingPolicyToCBOR :: MintingPolicy -> Text
mintingPolicyToCBOR = scriptToCBOR

----------------------- From CBOR -----------------------

scriptFromCBOR :: Coercible Script script => Text -> Maybe script
scriptFromCBOR = coerce . fmap (deserialise @Script . LBS.fromStrict) . decodeHex

validatorFromCBOR :: Text -> Maybe Validator
validatorFromCBOR = scriptFromCBOR

unsafeValidatorFromCBOR :: Text -> Validator
unsafeValidatorFromCBOR = fromJust . validatorFromCBOR

mintingPolicyFromCBOR :: Text -> Maybe MintingPolicy
mintingPolicyFromCBOR = scriptFromCBOR

unsafeMintingPolicyFromCBOR :: Text -> MintingPolicy
unsafeMintingPolicyFromCBOR = fromJust . mintingPolicyFromCBOR

--------------- Parameterized from CBOR ------------------

-- TODO: figure out how to do this in new plutus.

-- parameterizedScriptFromCBOR :: (ToData par, Coercible UPLCProgram script)
--     => Text -> par -> Maybe script
-- parameterizedScriptFromCBOR txt par = do
--     bs <- decodeHex txt
--     let program = uncheckedDeserialiseUPLC $ SBS.toShort bs
--         p = Program () latestVersion . mkConstant ()
--     eitherToMaybe $ coerce <$> program `applyProgram` p (toData par)

-- parameterizedValidatorFromCBOR :: (ToData par) => Text -> par -> Maybe Validator
-- parameterizedValidatorFromCBOR = parameterizedScriptFromCBOR

-- unsafeParameterizedValidatorFromCBOR :: (ToData par) => Text -> par -> Validator
-- unsafeParameterizedValidatorFromCBOR txt = fromJust . parameterizedValidatorFromCBOR txt

-- parameterizedMintingPolicyFromCBOR :: (ToData par) => Text -> par -> Maybe MintingPolicy
-- parameterizedMintingPolicyFromCBOR txt = fromJust . parameterizedScriptFromCBOR txt

-- unsafeParameterizedMintingPolicyFromCBOR :: (ToData a) => Text -> a -> MintingPolicy
-- unsafeParameterizedMintingPolicyFromCBOR = fromJust . parameterizedMintingPolicyFromCBOR
