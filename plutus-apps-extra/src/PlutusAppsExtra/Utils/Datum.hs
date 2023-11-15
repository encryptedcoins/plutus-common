{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusAppsExtra.Utils.Datum where

import qualified Cardano.Ledger.Alonzo.Data          as Alonzo
import           Cardano.Ledger.Alonzo.TxInfo        (transDataHash')
import           Cardano.Ledger.Crypto               (StandardCrypto)
import           Ledger                              (DatumFromQuery (..), DatumHash, datumHash)
import           Ledger.Tx.Constraints.TxConstraints (TxOutDatum (..))
import           Ouroboros.Consensus.Shelley.Eras    (ShelleyEra)
import           Plutus.ChainIndex                   (OutputDatum (..))
import           Plutus.V1.Ledger.Scripts            (Datum (..))
import           Plutus.V2.Ledger.Api                (builtinDataToData)
import           PlutusTx.IsData.Class               (ToData (toBuiltinData))
import           PlutusTx.Prelude                    (Bool (False), Eq ((==)), ($), (.))

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

hashDatum :: Datum -> DatumHash
hashDatum = transDataHash' . Alonzo.hashData @(ShelleyEra StandardCrypto) . Alonzo.Data . builtinDataToData . getDatum