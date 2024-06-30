{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusAppsExtra.Utils.Scripts where

import           Cardano.Address.Style.Shelley (unsafeFromRight)
import           Codec.Serialise               (deserialise, serialise)
import           Control.FromSum               (maybeToEither)
import           Data.Bifunctor                (Bifunctor (..))
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Short         as SBS
import           Data.Coerce                   (Coercible, coerce)
import           Data.Maybe                    (fromJust)
import           Ledger                        (MintingPolicy (..), Script (..), Validator (..))
import           PlutusCore                    (DefaultFun, DefaultUni)
import qualified PlutusCore                    as Plutus
import           PlutusCore.Error              (ApplyProgramError)
import           PlutusCore.MkPlc              (mkConstant)
import           PlutusLedgerApi.V3            (SerialisedScript, ToData, serialiseUPLC, toData, uncheckedDeserialiseUPLC)
import           Text.Hex                      (Text, decodeHex, encodeHex)
import           UntypedPlutusCore             (DeBruijn, Program (..), applyProgram)

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

data ParameterizedScriptFromCborError
    = CborDecodingError Text
    | ApplyProgramError ApplyProgramError
    deriving (Show)

parameterizedScriptFromCBOR :: (ToData par, Coercible SerialisedScript script)
    => Plutus.Version -> Text -> par -> Either ParameterizedScriptFromCborError script
parameterizedScriptFromCBOR pv txt par = do
    bs <- maybeToEither (CborDecodingError txt) $ decodeHex txt
    let programFun = uncheckedDeserialiseUPLC $ SBS.toShort bs
        programPar = Program () pv $ mkConstant () (toData par)
    program <- first ApplyProgramError $ programFun `applyProgram` programPar
    pure $ coerce $ serialiseUPLC program

unsafeParametrizedScriptFromCBOR :: (ToData par)
    => Plutus.Version -> Text -> par -> Script
unsafeParametrizedScriptFromCBOR pv txt = unsafeFromRight . parameterizedScriptFromCBOR pv txt

parameterizedValidatorFromCBOR :: forall par. (ToData par)
    => Plutus.Version -> Text -> par -> Either ParameterizedScriptFromCborError Validator
parameterizedValidatorFromCBOR = parameterizedScriptFromCBOR

unsafeParameterizedValidatorFromCBOR :: (ToData par) => Plutus.Version -> Text -> par -> Validator
unsafeParameterizedValidatorFromCBOR pv txt = unsafeFromRight . parameterizedValidatorFromCBOR pv txt

parameterizedMintingPolicyFromCBOR :: (ToData par) => Plutus.Version -> Text -> par -> Either ParameterizedScriptFromCborError MintingPolicy
parameterizedMintingPolicyFromCBOR = parameterizedScriptFromCBOR

unsafeParameterizedMintingPolicyFromCBOR :: (ToData a) => Plutus.Version -> Text -> a -> MintingPolicy
unsafeParameterizedMintingPolicyFromCBOR pv txt par = unsafeFromRight $ parameterizedMintingPolicyFromCBOR pv txt par
