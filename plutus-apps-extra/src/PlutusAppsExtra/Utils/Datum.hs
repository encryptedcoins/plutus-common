{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlutusAppsExtra.Utils.Datum where

import           Ledger                           (DatumFromQuery (..), DatumHash, datumHash)
import           Ledger.Constraints.TxConstraints (TxOutDatum (..))
import           Plutus.ChainIndex                (OutputDatum (..))
import           Plutus.V1.Ledger.Scripts         (Datum (..))
import           PlutusTx.IsData.Class            (ToData (toBuiltinData))
import           PlutusTx.Prelude                 (Bool (False), Eq ((==)), ($), (.))

toDatumHash :: ToData datum => datum -> TxOutDatum Datum
toDatumHash = TxOutDatumHash . Datum . toBuiltinData

toInlineDatum :: ToData datum => datum -> TxOutDatum Datum
toInlineDatum = TxOutDatumInline . Datum . toBuiltinData

{-# INLINABLE isInlineUnit #-}
isInlineUnit :: OutputDatum -> Bool
isInlineUnit (OutputDatum (Datum d)) = d == toBuiltinData ()
isInlineUnit _ = False

hashedUnit :: TxOutDatum Datum
hashedUnit = toDatumHash ()

inlinedUnit :: TxOutDatum Datum
inlinedUnit = toInlineDatum ()

inlinedUnitInTxOut :: (DatumHash, DatumFromQuery)
inlinedUnitInTxOut = (unitHash, DatumInline (Datum $ toBuiltinData ()))

unitHash :: DatumHash
unitHash = datumHash $ Datum $ toBuiltinData ()