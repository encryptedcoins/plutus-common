{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusAppsExtra.Utils.Datum where

import           Data.Text                                (Text)
import           Ledger                                   (Datum (..), DatumFromQuery (..), DatumHash (..), datumHash)
import           PlutusAppsExtra.PlutusApps.TxConstraints (TxOutDatum (..))
import           PlutusLedgerApi.V3                       (BuiltinByteString, OutputDatum (..), fromBuiltin)
import           PlutusPrelude                            (coerce)
import           PlutusTx.Builtins                        (serialiseData)
import           PlutusTx.IsData.Class                    (ToData (toBuiltinData))
import           PlutusTx.Prelude                         (Bool (False), Eq ((==)), blake2b_224, ($), (.))
import qualified Text.Hex                                 as T

toDatumHash :: ToData datum => datum -> TxOutDatum Datum
toDatumHash = TxOutDatumHash . Datum . toBuiltinData

toInlineDatum :: ToData datum => datum -> TxOutDatum Datum
toInlineDatum = TxOutDatumInline . Datum . toBuiltinData

{-# INLINABLE isInlineUnit #-}
isInlineUnit :: OutputDatum -> Bool
isInlineUnit (OutputDatum (Datum d)) = d == toBuiltinData ()
isInlineUnit _                       = False

hashedUnit :: TxOutDatum Datum
hashedUnit = $([|toDatumHash ()|])

inlinedUnit :: TxOutDatum Datum
inlinedUnit = $([|toInlineDatum ()|])

inlinedUnitInTxOut :: (DatumHash, DatumFromQuery)
inlinedUnitInTxOut = $([|(unitHash, DatumInline (Datum $ toBuiltinData ()))|])

unitHash :: DatumHash
unitHash = $([|datumHash $ Datum $ toBuiltinData ()|])

unitHashText :: Text
unitHashText = $([|T.encodeHex $ fromBuiltin @BuiltinByteString $ coerce unitHash|])

hashDatum :: Datum -> DatumHash
hashDatum = DatumHash . blake2b_224 . serialiseData . getDatum
