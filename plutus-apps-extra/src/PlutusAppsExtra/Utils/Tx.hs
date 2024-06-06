{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.Utils.Tx where

import qualified Cardano.Api                       as C
import           Cardano.Api.Shelley               (AnyCardanoEra (..), AsType (..), CardanoEra (..), ConsensusMode (..),
                                                    EraInMode (..), InAnyCardanoEra (..), KeyWitness (..), SerialiseAsCBOR (..),
                                                    ShelleyBasedEra (..), ToJSON, Tx (..), TxMetadataJsonSchema, metadataFromJson,
                                                    toEraInMode)
import qualified Cardano.Crypto.DSIGN              as Crypto
import           Cardano.Ledger.Shelley.API        (VKey (VKey), WitVKey (WitVKey))
import           Cardano.Node.Emulator.Params      (Params)
import           Control.FromSum                   (eitherToMaybe, fromMaybe)
import           Data.Aeson                        (ToJSON (..))
import           Data.Aeson.Extras                 (encodeByteString, tryDecode)
import           Data.Functor                      ((<&>))
import qualified Data.Map                          as Map
import           Data.Text                         (Text)
import           Ledger                            (PaymentPubKeyHash (..), PubKey (..), PubKeyHash (..), Signature (..), TxId,
                                                    TxOut (getTxOut), getCardanoTxProducedOutputs)
import           Ledger.Tx                         (CardanoTx (..))
import           Ledger.Tx.CardanoAPI              (CardanoBuildTx (..), getRequiredSigners)
import           PlutusAppsExtra.PlutusApps        (UnbalancedTx)
import           Plutus.V1.Ledger.Bytes            (bytes, fromBytes)
import           Plutus.V2.Ledger.Api              (BuiltinByteString, fromBuiltin, toBuiltin)
import           Text.Hex                          (decodeHex)

------------------------ Export/Import of transactions -------------------------

-- unbalancedTxToCBOR :: Params -> UnbalancedTx -> Maybe Text
-- unbalancedTxToCBOR params =
--     fmap (encodeByteString . serialiseToCBOR . partialTx) . eitherToMaybe . export params

textToCardanoTx :: Text -> Maybe CardanoTx
textToCardanoTx txt = do
    bs <- eitherToMaybe $ tryDecode txt
    tx <- eitherToMaybe $ deserialiseFromCBOR (AsTx AsBabbageEra) bs
    return $ CardanoTx tx BabbageEraInCardanoMode

cardanoTxToText :: CardanoTx -> Maybe Text
cardanoTxToText (CardanoTx tx BabbageEraInCardanoMode) = Just $ encodeByteString $ serialiseToCBOR tx
cardanoTxToText _ = Nothing

------------------------ Metadata -------------------------

addMetadataToCardanoBuildTx :: Maybe (C.TxMetadataInEra C.BabbageEra) -> CardanoBuildTx -> CardanoBuildTx
addMetadataToCardanoBuildTx mbMetadata (CardanoBuildTx txBody)
    = CardanoBuildTx (txBody{C.txMetadata = fromMaybe C.TxMetadataNone mbMetadata})

mkCip20Metadata :: [Text] -> Either C.TxMetadataJsonError (C.TxMetadataInEra C.BabbageEra)
mkCip20Metadata msgs = mkJsonMetadataNoSchema (Map.fromList [("674" :: Text, Map.fromList [("msg" :: Text, msgs)])])

mkJsonMetadataNoSchema :: ToJSON a => a -> Either C.TxMetadataJsonError (C.TxMetadataInEra C.BabbageEra)
mkJsonMetadataNoSchema = mkJsonMetadata C.TxMetadataJsonNoSchema

mkJsonMetadataDetailedSchema :: ToJSON a => a -> Either C.TxMetadataJsonError (C.TxMetadataInEra C.BabbageEra)
mkJsonMetadataDetailedSchema = mkJsonMetadata C.TxMetadataJsonDetailedSchema

mkJsonMetadata :: ToJSON a => TxMetadataJsonSchema -> a -> Either C.TxMetadataJsonError (C.TxMetadataInEra C.BabbageEra)
mkJsonMetadata schema val = C.TxMetadataInEra C.TxMetadataInBabbageEra <$> metadataFromJson schema (toJSON val)

------------------------ External keys and signatures -------------------------

textToPubkey :: Text -> Maybe PubKey
textToPubkey txt = PubKey . fromBytes <$> decodeHex txt

textToSignature :: Text -> Maybe Signature
textToSignature txt = Signature . toBuiltin <$> decodeHex txt

addCardanoTxSignature :: PubKey -> Signature -> CardanoTx -> CardanoTx
addCardanoTxSignature pubKey sig (CardanoTx (Tx body wits) eraInMode) = CardanoTx (Tx body wits') eraInMode
    where
        wits' = wits <> [ShelleyKeyWitness shelleyEra witVKey]
        witVKey = WitVKey (VKey vk) sig'
        shelleyEra = case eraInMode of
            ByronEraInCardanoMode   -> error "addCardanoTxSignature: Byron era isn't ShelleyBasedEra."
            ShelleyEraInCardanoMode -> ShelleyBasedEraShelley
            AllegraEraInCardanoMode -> ShelleyBasedEraAllegra
            MaryEraInCardanoMode    -> ShelleyBasedEraMary
            AlonzoEraInCardanoMode  -> ShelleyBasedEraAlonzo
            BabbageEraInCardanoMode -> ShelleyBasedEraBabbage

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

getTxDatums :: CardanoTx -> [C.TxOutDatum C.CtxTx C.BabbageEra]
getTxDatums = map ((\(C.TxOut _ _ c _) -> c) . getTxOut) . Map.elems . getCardanoTxProducedOutputs

-- txIsSignedByKey :: TxId -> BuiltinByteString -> IO Bool
-- txIsSignedByKey txId pkh = getTxFromId txId <&> \case
--     Just (CardanoTx tx BabbageEraInCardanoMode) -> PaymentPubKeyHash (PubKeyHash pkh) `elem` getRequiredSigners tx
--     _ -> False