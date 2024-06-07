{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusAppsExtra.Utils.Datum where

import qualified Cardano.Api           as C
import           Cardano.Api.Shelley   (TxOutDatum, fromAlonzoData)
import           Cardano.Ledger.Conway (Conway)
import           Cardano.Ledger.Plutus (Data (..))
import           Data.Coerce           (coerce)
import           Data.Text             (Text)
import           Ledger                (DatumFromQuery (..), DatumHash (..), datumHash)
import           PlutusLedgerApi.V3    (BuiltinByteString, Datum (..), OutputDatum (..), fromBuiltin, toData)
import           PlutusTx.Builtins     (serialiseData)
import           PlutusTx.IsData.Class (ToData (toBuiltinData))
import           PlutusTx.Prelude      (Bool (False), Eq ((==)), blake2b_224, ($), (.))
import qualified Text.Hex              as T

toDatumHash :: ToData datum => datum -> TxOutDatum C.CtxTx C.ConwayEra
toDatumHash = C.TxOutDatumHash C.AlonzoEraOnwardsConway . C.hashScriptDataBytes . fromAlonzoData . Data @Conway . toData

toInlineDatum :: ToData datum => datum -> TxOutDatum C.CtxTx C.ConwayEra
toInlineDatum = C.TxOutDatumInline C.BabbageEraOnwardsConway . fromAlonzoData . Data @Conway . toData

{-# INLINABLE isInlineUnit #-}
isInlineUnit :: OutputDatum -> Bool
isInlineUnit (OutputDatum (Datum d)) = d == toBuiltinData ()
isInlineUnit _                       = False

hashedUnit :: TxOutDatum C.CtxTx C.ConwayEra
hashedUnit = $([|toDatumHash ()|])

inlinedUnit :: TxOutDatum C.CtxTx C.ConwayEra
inlinedUnit = $([|toInlineDatum ()|])

inlinedUnitInTxOut :: (DatumHash, DatumFromQuery)
inlinedUnitInTxOut = $([|(unitHash, DatumInline (Datum $ toBuiltinData ()))|])

unitHash :: DatumHash
unitHash = $([|datumHash $ Datum $ toBuiltinData ()|])

unitHashText :: Text
unitHashText = $([|T.encodeHex $ fromBuiltin @BuiltinByteString $ coerce unitHash|])

hashDatum :: Datum -> DatumHash
hashDatum = DatumHash . blake2b_224 . serialiseData . getDatum
