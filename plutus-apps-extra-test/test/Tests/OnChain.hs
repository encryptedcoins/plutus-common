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

module Tests.OnChain where

import           Ledger.Typed.Scripts                 (IsScriptContext (..), MintingPolicy)
import           Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes (..), mkTypedValidator)
import           PlutusTx                             (compile)
import           PlutusTx.Prelude                     (Bool (..), BuiltinByteString, map, ($))

import           Ledger                               (mkMintingPolicyScript)
import           PlutusAppsExtra.Constraints.OnChain  (tokensMinted)
import           PlutusLedgerApi.V2                   (ScriptContext (..), TokenName (..))
import qualified PlutusLedgerApi.V3                   as V3

------------------------------------- Test Minting Policy --------------------------------------

{-# INLINABLE testTokenName #-}
testTokenName :: BuiltinByteString -> TokenName
testTokenName = TokenName

testPolicyCheck :: [BuiltinByteString] -> V3.ScriptContext -> Bool
testPolicyCheck bss ctx = cond1
  where
    names = map testTokenName bss

    cond1 = tokensMinted ctx $ V3.unsafeFromList $ map (, 1) names

testPolicy :: MintingPolicy
testPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy testPolicyCheck ||])

------------------------------------- Test Validator --------------------------------------

data Testing
instance ValidatorTypes Testing where
  type instance DatumType Testing = ()
  type instance RedeemerType Testing = ()

{-# INLINABLE testValidatorCheck #-}
testValidatorCheck :: () -> () -> ScriptContext -> Bool
testValidatorCheck _ _ _ = True

testTypedValidator :: TypedValidator Testing
testTypedValidator = mkTypedValidator @Testing
    $$(PlutusTx.compile [|| testValidatorCheck ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator @ScriptContext @() @()
