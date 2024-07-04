{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusAppsExtra.Utils.Scripts
    ( scriptToCBOR
    , validatorToCBOR
    , mintingPolicyToCBOR
      -- fromCBOR
    , progFromCBOR
    , scriptFromCBOR
    , validatorFromCBOR
    , unsafeValidatorFromCBOR
    , mintingPolicyFromCBOR
    , unsafeMintingPolicyFromCBOR
    , parameterizedScriptFromCBOR
    , unsafeParametrizedScriptFromCBOR
    , parameterizedValidatorFromCBOR
    , unsafeParameterizedValidatorFromCBOR
    , parameterizedMintingPolicyFromCBOR
    , unsafeParameterizedMintingPolicyFromCBOR
      -- useful reexports
    , Language (..)
    ) where

import           Cardano.Address.Style.Shelley (unsafeFromRight)
import           Cardano.Ledger.Alonzo.Core    (eraProtVerLow)
import           Cardano.Ledger.Core           (Era)
import           Cardano.Ledger.Plutus         (Language (..), Plutus (..), PlutusBinary (..), PlutusLanguage (..), PlutusRunnable (..))
import           Codec.Serialise               (deserialise, serialise)
import           Control.Arrow                 (ArrowChoice (..))
import           Control.FromSum               (maybeToEither)
import           Control.Lens                  (over)
import           Data.Bifunctor                (Bifunctor (..))
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Short         as SBS
import           Data.Coerce                   (Coercible, coerce)
import           Data.Maybe                    (fromJust)
import           Ledger                        (MintingPolicy (..), Script (..), Validator (..))
import qualified Plutus.Script.Utils.Typed     as Plutus
import           PlutusCore                    (DefaultFun, DefaultUni)
import           PlutusCore.Error              (ApplyProgramError)
import           PlutusCore.MkPlc              (mkConstant)
import           PlutusLedgerApi.Common        (ScriptDecodeError, ScriptNamedDeBruijn (..), deserialisedScript, serialiseUPLC)
import           PlutusLedgerApi.V3            (SerialisedScript, ToData, toData)
import           Text.Hex                      (Text, decodeHex, encodeHex)
import qualified UntypedPlutusCore             as UPLC
import           UntypedPlutusCore             (DeBruijn, Program (..), applyProgram)

----------------------- To CBOR -----------------------

scriptToCBOR :: Coercible Script script => script -> Text
scriptToCBOR = encodeHex . LBS.toStrict . serialise . coerce @_ @Script

validatorToCBOR :: Validator -> Text
validatorToCBOR = scriptToCBOR

mintingPolicyToCBOR :: MintingPolicy -> Text
mintingPolicyToCBOR = scriptToCBOR

----------------------- From CBOR -----------------------

data ScriptFromCborError
    = CborDecodeError Text
    | ScriptDecodeError ScriptDecodeError
    | ApplyProgramError ApplyProgramError
    deriving (Show)

progFromCBOR :: forall (lang :: Plutus.Language) era.
    ( PlutusLanguage lang
    , Era era
    ) => Text -> Either ScriptFromCborError (Program DeBruijn DefaultUni DefaultFun ())
progFromCBOR txt = do
    sbs <- maybeToEither (CborDecodeError txt) $ SBS.toShort <$> decodeHex txt
    bimap
        ScriptDecodeError
        ((\(ScriptNamedDeBruijn s) -> toNameless s) . deserialisedScript . plutusRunnable)
        $ decodePlutusRunnable @lang (eraProtVerLow @era) $ Plutus $ PlutusBinary sbs
  where
    toNameless = over UPLC.progTerm $ UPLC.termMapNames UPLC.unNameDeBruijn

scriptFromCBOR :: forall (lang :: Plutus.Language) era script.
    ( PlutusLanguage lang
    , Era era
    , Coercible Script script
    ) => Text -> Either ScriptFromCborError script
scriptFromCBOR = fmap (coerce . serialiseUPLC) . progFromCBOR @lang @era

validatorFromCBOR :: forall (lang :: Plutus.Language) era.
    ( PlutusLanguage lang
    , Era era
    ) => Text -> Either ScriptFromCborError Validator
validatorFromCBOR = scriptFromCBOR @lang @era

unsafeValidatorFromCBOR :: forall (lang :: Plutus.Language) era.
    ( PlutusLanguage lang
    , Era era
    ) => Text -> Validator
unsafeValidatorFromCBOR = either (error . show) id . validatorFromCBOR @lang @era

mintingPolicyFromCBOR :: forall (lang :: Plutus.Language) era.
    ( PlutusLanguage lang
    , Era era
    ) => Text -> Either ScriptFromCborError MintingPolicy
mintingPolicyFromCBOR = scriptFromCBOR @lang @era

unsafeMintingPolicyFromCBOR :: forall (lang :: Plutus.Language) era.
    ( PlutusLanguage lang
    , Era era
    ) => Text -> MintingPolicy
unsafeMintingPolicyFromCBOR = either (error . show) id . mintingPolicyFromCBOR @lang @era

parameterizedScriptFromCBOR :: forall (lang :: Plutus.Language) era par script.
    ( PlutusLanguage lang
    , Era era
    , ToData par
    , Coercible SerialisedScript script
    ) => Text -> par -> Either ScriptFromCborError script
parameterizedScriptFromCBOR txt par = do
    progFun <- progFromCBOR @lang @era txt
    let progPar =  Program () (_progVer progFun) $ mkConstant () (toData par)
    prog <- first ApplyProgramError $ progFun `applyProgram` progPar
    pure $ coerce $ serialiseUPLC prog

unsafeParametrizedScriptFromCBOR :: forall (lang :: Plutus.Language) era par.
    (PlutusLanguage lang
    , Era era
    , ToData par
    ) => Text -> par -> Script
unsafeParametrizedScriptFromCBOR txt = either (error . show) id . parameterizedScriptFromCBOR @lang @era txt

parameterizedValidatorFromCBOR :: forall (lang :: Plutus.Language) era par.
    (PlutusLanguage lang
    , Era era
    , ToData par
    ) => Text -> par -> Either ScriptFromCborError Validator
parameterizedValidatorFromCBOR = parameterizedScriptFromCBOR @lang @era

unsafeParameterizedValidatorFromCBOR :: forall (lang :: Plutus.Language) era par.
    ( PlutusLanguage lang
    , Era era
    , ToData par
    ) => Text -> par -> Validator
unsafeParameterizedValidatorFromCBOR txt = either (error . show) id . parameterizedValidatorFromCBOR @lang @era txt

parameterizedMintingPolicyFromCBOR :: forall (lang :: Plutus.Language) era par.
    ( PlutusLanguage lang
    , Era era
    , ToData par
    ) => Text -> par -> Either ScriptFromCborError MintingPolicy
parameterizedMintingPolicyFromCBOR = parameterizedScriptFromCBOR @lang @era

unsafeParameterizedMintingPolicyFromCBOR :: forall (lang :: Plutus.Language) era par.
    ( PlutusLanguage lang
    , Era era
    , ToData par
    ) => Text -> par -> MintingPolicy
unsafeParameterizedMintingPolicyFromCBOR txt par = either (error . show) id $ parameterizedMintingPolicyFromCBOR @lang @era txt par
