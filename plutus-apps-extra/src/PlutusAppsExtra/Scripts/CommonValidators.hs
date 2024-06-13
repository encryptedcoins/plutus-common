{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module PlutusAppsExtra.Scripts.CommonValidators where

import           Ledger                         (mkValidatorScript)
import           Ledger.Typed.Scripts           (IsScriptContext (..), Language (..), Validator, Versioned (..))
import           Plutus.Script.Utils.V2.Address (mkValidatorAddress)
import           Plutus.Script.Utils.V2.Scripts (ValidatorHash, validatorHash)
import           PlutusCore                     (latestVersion)
import           PlutusLedgerApi.V3             (Address, ScriptContext)
import           PlutusTx                       (compile, liftCode, unsafeApplyCode)
import           PlutusTx.Prelude               (Bool (False), Integer, flip, ($), (.))

{-# INLINABLE alwaysFalseValidatorCheck #-}
alwaysFalseValidatorCheck :: Integer -> () -> () -> ScriptContext -> Bool
alwaysFalseValidatorCheck _ _ _ _ = False

alwaysFalseValidator :: Integer -> Validator
alwaysFalseValidator salt = mkValidatorScript $
  $$(PlutusTx.compile [|| mkUntypedValidator . alwaysFalseValidatorCheck ||])
    `PlutusTx.unsafeApplyCode`
        PlutusTx.liftCode latestVersion salt

alwaysFalseValidatorV :: Integer -> Versioned Validator
alwaysFalseValidatorV = flip Versioned PlutusV2 . alwaysFalseValidator

alwaysFalseValidatorHash :: Integer -> ValidatorHash
alwaysFalseValidatorHash = validatorHash . alwaysFalseValidator

alwaysFalseValidatorAddress :: Integer -> Address
alwaysFalseValidatorAddress = mkValidatorAddress . alwaysFalseValidator
