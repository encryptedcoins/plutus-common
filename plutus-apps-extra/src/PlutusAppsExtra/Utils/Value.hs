{-# LANGUAGE NoImplicitPrelude #-}

module PlutusAppsExtra.Utils.Value where

import           PlutusTx.AssocMap (singleton, lookup)
import           PlutusTx.Prelude
import           Ledger.Value      (CurrencySymbol, Value (..), TokenName, adaOnlyValue)
import qualified Ledger.Value      as Value

unflattenValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
unflattenValue = sum . map (\(s, n, i) -> Value.singleton s n i)

currencyOnlyValue :: CurrencySymbol -> Value -> Value
currencyOnlyValue symb = maybe zero (Value . singleton symb) . lookup symb . getValue

isCurrencyOnlyValue :: CurrencySymbol -> Value -> Bool
isCurrencyOnlyValue symb val = val == currencyOnlyValue symb val

currencyAndAdaOnlyValue :: CurrencySymbol -> Value -> Value
currencyAndAdaOnlyValue symb val = adaOnlyValue val + currencyOnlyValue symb val

isCurrencyAndAdaOnlyValue :: CurrencySymbol -> Value -> Bool
isCurrencyAndAdaOnlyValue symb val = val == currencyAndAdaOnlyValue symb val