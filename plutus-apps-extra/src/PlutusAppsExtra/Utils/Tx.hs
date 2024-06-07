{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.Utils.Tx where

import qualified Cardano.Api                as C
import           Cardano.Api.Shelley        (AsType (..), KeyWitness (..), SerialiseAsCBOR (..), ShelleyBasedEra (..), ToJSON, Tx (..),
                                             TxMetadataInEra (..), TxMetadataJsonSchema, metadataFromJson)
import qualified Cardano.Crypto.DSIGN       as Crypto
import           Cardano.Ledger.Shelley.API (VKey (VKey), WitVKey (WitVKey))
import           Control.FromSum            (eitherToMaybe, fromMaybe)
import           Data.Aeson                 (ToJSON (..))
import           Data.Aeson.Extras          (tryDecode)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           Ledger                     (PubKey (..), Signature (..), TxOut (getTxOut), getCardanoTxProducedOutputs)
import           Ledger.Tx                  (CardanoTx (..))
import           Ledger.Tx.CardanoAPI       (CardanoBuildTx (..))
import           PlutusLedgerApi.V1.Bytes   (bytes, encodeByteString)
import           PlutusLedgerApi.V3         (fromBuiltin, fromBytes, toBuiltin)
import           Text.Hex                   (decodeHex)

------------------------ Export/Import of transactions -------------------------

-- unbalancedTxToCBOR :: Params -> UnbalancedTx -> Maybe Text
-- unbalancedTxToCBOR params =
--     fmap (encodeByteString . serialiseToCBOR . partialTx) . eitherToMaybe . export params

textToCardanoTx :: Text -> Maybe CardanoTx
textToCardanoTx txt = do
    bs <- eitherToMaybe $ tryDecode txt
    tx <- eitherToMaybe $ deserialiseFromCBOR (AsTx AsConwayEra) bs
    return $ CardanoTx tx ShelleyBasedEraConway

cardanoTxToText :: CardanoTx -> Maybe Text
cardanoTxToText (CardanoTx tx ShelleyBasedEraBabbage) = Just $ encodeByteString $ serialiseToCBOR tx
cardanoTxToText (CardanoTx tx ShelleyBasedEraShelley) = Just $ encodeByteString $ serialiseToCBOR tx
cardanoTxToText (CardanoTx tx ShelleyBasedEraAllegra) = Just $ encodeByteString $ serialiseToCBOR tx
cardanoTxToText (CardanoTx tx ShelleyBasedEraMary)    = Just $ encodeByteString $ serialiseToCBOR tx
cardanoTxToText (CardanoTx tx ShelleyBasedEraAlonzo)  = Just $ encodeByteString $ serialiseToCBOR tx
cardanoTxToText (CardanoTx tx ShelleyBasedEraConway)  = Just $ encodeByteString $ serialiseToCBOR tx

------------------------ Metadata -------------------------

addMetadataToCardanoBuildTx :: Maybe (C.TxMetadataInEra C.ConwayEra) -> CardanoBuildTx -> CardanoBuildTx
addMetadataToCardanoBuildTx mbMetadata (CardanoBuildTx txBody)
    = CardanoBuildTx (txBody{C.txMetadata = fromMaybe C.TxMetadataNone mbMetadata})

mkCip20Metadata :: [Text] -> Either C.TxMetadataJsonError (C.TxMetadataInEra C.ConwayEra)
mkCip20Metadata msgs = mkJsonMetadataNoSchema (Map.fromList [("674" :: Text, Map.fromList [("msg" :: Text, msgs)])])

mkJsonMetadataNoSchema :: ToJSON a => a -> Either C.TxMetadataJsonError (C.TxMetadataInEra C.ConwayEra)
mkJsonMetadataNoSchema = mkJsonMetadata C.TxMetadataJsonNoSchema

mkJsonMetadataDetailedSchema :: ToJSON a => a -> Either C.TxMetadataJsonError (C.TxMetadataInEra C.ConwayEra)
mkJsonMetadataDetailedSchema = mkJsonMetadata C.TxMetadataJsonDetailedSchema

mkJsonMetadata :: ToJSON a => TxMetadataJsonSchema -> a -> Either C.TxMetadataJsonError (C.TxMetadataInEra C.ConwayEra)
mkJsonMetadata schema val = TxMetadataInEra ShelleyBasedEraConway <$> metadataFromJson schema (toJSON val)

------------------------ External keys and signatures -------------------------

textToPubkey :: Text -> Maybe PubKey
textToPubkey txt = PubKey . fromBytes <$> decodeHex txt

textToSignature :: Text -> Maybe Signature
textToSignature txt = Signature . toBuiltin <$> decodeHex txt

addCardanoTxSignature :: PubKey -> Signature -> CardanoTx -> CardanoTx
addCardanoTxSignature pubKey sig (CardanoTx (Tx body wits) shelleyBasedEra) = CardanoTx (Tx body wits') shelleyBasedEra
    where
        wits' = wits <> [ShelleyKeyWitness shelleyBasedEra witVKey]
        witVKey = WitVKey (VKey vk) sig'

        vk = fromMaybe (error "addCardanoTxSignature: deserialise VerKeyDSIGN from a PubKey.")
            . Crypto.rawDeserialiseVerKeyDSIGN
            . bytes
            $ getPubKey pubKey

        sig' = Crypto.SignedDSIGN
            . fromMaybe (error "addCardanoTxSignature: deserialise SigDSIGN from a Signature.")
            . Crypto.rawDeserialiseSigDSIGN
            . fromBuiltin
            $ getSignature sig

------------------------------------- Other -------------------------------------

getTxDatums :: CardanoTx -> [C.TxOutDatum C.CtxTx C.ConwayEra]
getTxDatums = map ((\(C.TxOut _ _ c _) -> c) . getTxOut) . Map.elems . getCardanoTxProducedOutputs

-- txIsSignedByKey :: TxId -> BuiltinByteString -> IO Bool
-- txIsSignedByKey txId pkh = getTxFromId txId <&> \case
--     Just (CardanoTx tx BabbageEraInCardanoMode) -> PaymentPubKeyHash (PubKeyHash pkh) `elem` getRequiredSigners tx
--     _ -> False