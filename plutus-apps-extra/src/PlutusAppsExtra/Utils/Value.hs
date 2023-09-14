{-# LANGUAGE NoImplicitPrelude #-}

module PlutusAppsExtra.Utils.Value where

import           Plutus.Script.Utils.Value (CurrencySymbol, TokenName, adaOnlyValue)
import qualified Plutus.Script.Utils.Value as P
import           PlutusTx.AssocMap         (lookup, singleton)
import           PlutusTx.Prelude

unflattenValue :: [(CurrencySymbol, TokenName, Integer)] -> P.Value
unflattenValue = sum . map (\(s, n, i) -> P.singleton s n i)

currencyOnlyValue :: CurrencySymbol -> P.Value -> P.Value
currencyOnlyValue symb = maybe zero (P.Value . singleton symb) . lookup symb . P.getValue

isCurrencyOnlyValue :: CurrencySymbol -> P.Value -> Bool
isCurrencyOnlyValue symb val = val == currencyOnlyValue symb val

currencyAndAdaOnlyValue :: CurrencySymbol -> P.Value -> P.Value
currencyAndAdaOnlyValue symb val = adaOnlyValue val + currencyOnlyValue symb val

isCurrencyAndAdaOnlyValue :: CurrencySymbol -> P.Value -> Bool
isCurrencyAndAdaOnlyValue symb val = val == currencyAndAdaOnlyValue symb val