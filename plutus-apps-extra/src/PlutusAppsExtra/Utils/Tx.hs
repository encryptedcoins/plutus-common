{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.Utils.Tx where

import qualified Cardano.Api                            as C
import           Cardano.Api.Byron                      (Tx (ByronTx))
import           Cardano.Api.Shelley                    (AnyCardanoEra (..), AsType (..), CardanoEra (..), ConsensusMode (..),
                                                         EraInMode (..), InAnyCardanoEra (..), KeyWitness (..), SerialiseAsCBOR (..),
                                                         ShelleyBasedEra (..), ToJSON, Tx (..), TxMetadataJsonSchema, metadataFromJson,
                                                         toEraInMode, toShelleyMetadata)
import           Cardano.Chain.UTxO                     (ATxAux (..))
import qualified Cardano.Crypto.DSIGN                   as Crypto
import qualified Cardano.Ledger.Alonzo.Data             as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx               as Alonzo
import           Cardano.Ledger.Babbage.Tx              (ValidatedTx (ValidatedTx))
import           Cardano.Ledger.Shelley.API             (VKey (VKey), WitVKey (WitVKey))
import qualified Cardano.Ledger.Shelley.API.Types       as Shelley
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Allegra
import           Cardano.Node.Emulator.Params           (Params)
import           Cardano.Wallet.Api.Types               (ApiSerialisedTransaction (..), getApiT)
import           Cardano.Wallet.LocalClient.ExportTx    (ExportTx (..), export)
import           Cardano.Wallet.Primitive.Types.Tx      (SealedTx, cardanoTxIdeallyNoLaterThan, sealedTxFromCardano')
import           Control.FromSum                        (eitherToMaybe, fromMaybe)
import           Control.Monad.Catch                    (MonadThrow (throwM))
import           Data.Aeson                             (ToJSON (..))
import           Data.Aeson.Extras                      (encodeByteString, tryDecode)
import           Data.Functor                           ((<&>))
import qualified Data.Map                               as Map
import           Data.Maybe.Strict                      (maybeToStrictMaybe)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Ledger                                 (PaymentPubKeyHash (..), PubKey (..), PubKeyHash (..), Signature (..), TxId,
                                                         TxOut (getTxOut), getCardanoTxProducedOutputs)
import           Ledger.Tx                              (CardanoTx (..))
import           Ledger.Tx.CardanoAPI                   (CardanoBuildTx (..), getRequiredSigners)
import           Ledger.Tx.Constraints                  (UnbalancedTx)
import           Plutus.V1.Ledger.Bytes                 (bytes, fromBytes)
import           Plutus.V2.Ledger.Api                   (BuiltinByteString, fromBuiltin, toBuiltin)
import           PlutusAppsExtra.IO.ChainIndex.Plutus   (getTxFromId)
import           PlutusAppsExtra.Types.Error            (MkTxError (..))
import           Text.Hex                               (decodeHex)

------------------------ Export/Import of transactions -------------------------

unbalancedTxToCBOR :: Params -> UnbalancedTx -> Maybe Text
unbalancedTxToCBOR params =
    fmap (encodeByteString . serialiseToCBOR . partialTx) . eitherToMaybe . export params

textToCardanoTx :: Text -> Maybe CardanoTx
textToCardanoTx txt = do
    bs <- eitherToMaybe $ tryDecode txt
    tx <- eitherToMaybe $ deserialiseFromCBOR (AsTx AsBabbageEra) bs
    return $ CardanoTx tx BabbageEraInCardanoMode

cardanoTxToText :: CardanoTx -> Maybe Text
cardanoTxToText (CardanoTx tx BabbageEraInCardanoMode) = Just $ encodeByteString $ serialiseToCBOR tx
cardanoTxToText _ = Nothing

apiSerializedTxToCardanoTx :: ApiSerialisedTransaction -> Maybe CardanoTx
apiSerializedTxToCardanoTx = toSomeTx . toAnyEraTx
    where
        toAnyEraTx = cardanoTxIdeallyNoLaterThan (AnyCardanoEra BabbageEra) . getApiT . transaction
        toSomeTx (InAnyCardanoEra cera tx) = CardanoTx tx <$> toEraInMode cera CardanoMode

cardanoTxToSealedTx :: CardanoTx -> SealedTx
cardanoTxToSealedTx (CardanoTx tx _) = sealedTxFromCardano' tx

addMetadataToCardanoBuildTx :: Maybe (C.TxMetadataInEra C.BabbageEra) -> CardanoBuildTx -> CardanoBuildTx
addMetadataToCardanoBuildTx mbMetadata (CardanoBuildTx txBody)
    = CardanoBuildTx (txBody{C.txMetadata = fromMaybe C.TxMetadataNone mbMetadata})

mkJsonMetadataNoSchema :: ToJSON a => a -> Either C.TxMetadataJsonError (C.TxMetadataInEra C.BabbageEra)
mkJsonMetadataNoSchema = mkJsonMetadata C.TxMetadataJsonNoSchema

mkJsonMetadataDetailedSchema :: ToJSON a => a -> Either C.TxMetadataJsonError (C.TxMetadataInEra C.BabbageEra)
mkJsonMetadataDetailedSchema = mkJsonMetadata C.TxMetadataJsonDetailedSchema

mkJsonMetadata :: ToJSON a => TxMetadataJsonSchema -> a -> Either C.TxMetadataJsonError (C.TxMetadataInEra C.BabbageEra)
mkJsonMetadata schema val = C.TxMetadataInEra C.TxMetadataInBabbageEra <$> metadataFromJson schema (toJSON val)

-- addMetadataToCardanoTx turned out to be useless due to the fact that adding the metadata must occur
-- before balancing the transaction. So it can be deleted in future.
addMetadataToCardanoTx :: (MonadThrow m, ToJSON a) => CardanoTx -> Maybe a -> TxMetadataJsonSchema -> m CardanoTx
addMetadataToCardanoTx ctx Nothing _ = pure ctx
addMetadataToCardanoTx ctx (Just val) schema = case metadataFromJson schema (toJSON val) of
    Left  err  -> throwM $ UnparsableMetadata $ T.pack $ show err
    Right meta -> pure $ addMetadataToCardanoTx' ctx meta

addMetadataToCardanoTx'  :: CardanoTx -> C.TxMetadata -> CardanoTx

addMetadataToCardanoTx' (CardanoTx (ByronTx (ATxAux tx wit _)) eraInMode) (C.TxMetadata meta)
    = CardanoTx (ByronTx (ATxAux tx wit (serialiseToCBOR $ C.TxMetadata meta))) eraInMode

addMetadataToCardanoTx' (CardanoTx (ShelleyTx ShelleyBasedEraShelley Shelley.Tx{..}) eraInMode) (C.TxMetadata meta)
    = let metadataShelley = maybeToStrictMaybe $ Just $ Shelley.Metadata $ toShelleyMetadata meta
      in CardanoTx (ShelleyTx C.ShelleyBasedEraShelley (Shelley.Tx{auxiliaryData = metadataShelley, ..})) eraInMode

addMetadataToCardanoTx' (CardanoTx (ShelleyTx ShelleyBasedEraAllegra Shelley.Tx{..}) eraInMode) (C.TxMetadata meta)
    = let meta' = toShelleyMetadata meta
          metadataAllegra = auxiliaryData <&> \(Allegra.AuxiliaryData a s) -> Allegra.AuxiliaryData (a <> meta') s
      in CardanoTx (ShelleyTx C.ShelleyBasedEraAllegra (Shelley.Tx{auxiliaryData = metadataAllegra, ..})) eraInMode

addMetadataToCardanoTx' (CardanoTx (ShelleyTx ShelleyBasedEraMary Shelley.Tx{..}) eraInMode) (C.TxMetadata meta)
    = let meta' = toShelleyMetadata meta
          metadataMarry = auxiliaryData <&> \(Allegra.AuxiliaryData a s) -> Allegra.AuxiliaryData (a <> meta') s
      in CardanoTx (ShelleyTx C.ShelleyBasedEraMary (Shelley.Tx{auxiliaryData = metadataMarry, ..})) eraInMode

addMetadataToCardanoTx' (CardanoTx (ShelleyTx ShelleyBasedEraAlonzo ValidatedTx{..}) eraInMode) (C.TxMetadata meta)
    = let metadataAlonzo = auxiliaryData <&> \Alonzo.AuxiliaryData{..} -> Alonzo.AuxiliaryData{txMD = toShelleyMetadata meta, ..}
      in CardanoTx (ShelleyTx C.ShelleyBasedEraAlonzo (ValidatedTx{Alonzo.auxiliaryData = metadataAlonzo, ..})) eraInMode

addMetadataToCardanoTx' (CardanoTx (ShelleyTx ShelleyBasedEraBabbage ValidatedTx{..}) eraInMode) (C.TxMetadata meta)
    = let metadataBabbage = auxiliaryData <&> \Alonzo.AuxiliaryData{..} -> Alonzo.AuxiliaryData{txMD = toShelleyMetadata meta, ..}
      in CardanoTx (ShelleyTx C.ShelleyBasedEraBabbage (ValidatedTx{Alonzo.auxiliaryData = metadataBabbage, ..})) eraInMode

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

txIsSignedByKey :: TxId -> BuiltinByteString -> IO Bool
txIsSignedByKey txId pkh = getTxFromId txId <&> \case
    Just (CardanoTx tx BabbageEraInCardanoMode) -> PaymentPubKeyHash (PubKeyHash pkh) `elem` getRequiredSigners tx
    _ -> False