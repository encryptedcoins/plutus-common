{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusAppsExtra.Utils.Scripts where

import           Codec.Serialise       (deserialise, serialise)
import           Control.FromSum       (eitherToMaybe)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Coerce           (Coercible, coerce)
import           Data.Maybe            (fromJust)
import           Ledger                (MintingPolicy (..), Script (..), Validator (..))
import           PlutusCore            (DefaultFun, DefaultUni, latestVersion)
import           PlutusCore.MkPlc      (mkConstant)
import           PlutusLedgerApi.V3    (SerialisedScript, ToData, serialiseUPLC, toData, uncheckedDeserialiseUPLC)
import           Text.Hex              (Text, decodeHex, encodeHex)
import           UntypedPlutusCore     (DeBruijn, Program (..), applyProgram)

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

parameterizedScriptFromCBOR :: (ToData par, Coercible SerialisedScript script)
    => Text -> par -> Maybe script
parameterizedScriptFromCBOR txt par = do
    bs <- decodeHex txt
    let programFun = uncheckedDeserialiseUPLC $ SBS.toShort bs
        programPar = Program () latestVersion $ mkConstant () (toData par)
    program <- eitherToMaybe $ programFun `applyProgram` programPar
    pure $ coerce $ serialiseUPLC program

parameterizedValidatorFromCBOR :: forall par. (ToData par) => Text -> par -> Maybe Validator
parameterizedValidatorFromCBOR = parameterizedScriptFromCBOR

unsafeParameterizedValidatorFromCBOR :: (ToData par) => Text -> par -> Validator
unsafeParameterizedValidatorFromCBOR txt = fromJust . parameterizedValidatorFromCBOR txt

parameterizedMintingPolicyFromCBOR :: (ToData par) => Text -> par -> Maybe MintingPolicy
parameterizedMintingPolicyFromCBOR = parameterizedScriptFromCBOR

unsafeParameterizedMintingPolicyFromCBOR :: (ToData a) => Text -> a -> MintingPolicy
unsafeParameterizedMintingPolicyFromCBOR txt par = fromJust $ parameterizedMintingPolicyFromCBOR txt par
