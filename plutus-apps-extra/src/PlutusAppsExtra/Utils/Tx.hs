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
import qualified Cardano.Api.Byron                      as Byron
import           Cardano.Api.Shelley                    (AnyCardanoEra (..), AsType (..), CardanoEra (..), ConsensusMode (..),
                                                         EraInMode (..), InAnyCardanoEra (..), SerialiseAsCBOR (..), ToJSON,
                                                         Tx (..), TxMetadataJsonSchema, metadataFromJson, toEraInMode,
                                                         toShelleyMetadata)
import           Cardano.Chain.UTxO                     (ATxAux (..))
import qualified Cardano.Crypto.DSIGN                   as Crypto
import qualified Cardano.Ledger.Alonzo.Data             as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx               as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness        as Wits
import           Cardano.Ledger.Babbage.Tx              (ValidatedTx (ValidatedTx))
import           Cardano.Ledger.Shelley.API             (VKey (VKey), WitVKey (WitVKey))
import qualified Cardano.Ledger.Shelley.API.Types       as Shelley
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Allegra
import           Cardano.Node.Emulator.Params           (Params)
import           Cardano.Wallet.Api.Types               (ApiSerialisedTransaction (..), getApiT)
import           Cardano.Wallet.LocalClient.ExportTx    (ExportTx (..), export)
import           Cardano.Wallet.Primitive.Types.Tx      (SealedTx, cardanoTxIdeallyNoLaterThan, sealedTxFromCardano')
import           Control.FromSum                        (eitherToMaybe, fromMaybe)
import           Control.Lens                           (At (at), (&), (?~))
import           Control.Monad.Catch                    (MonadThrow (throwM))
import           Data.Aeson                             (ToJSON (..))
import           Data.Aeson.Extras                      (encodeByteString, tryDecode)
import           Data.Functor                           ((<&>))
import           Data.Maybe.Strict                      (maybeToStrictMaybe)
import qualified Data.Set                               as Set
import           Data.Text                              (Text)
import           Ledger                                 (PubKey (..), Signature (..), Tx (..), cardanoTxMap, signatures, 
                                                         getCardanoTxProducedOutputs, TxOut (getTxOut))
import           Ledger.Constraints                     (UnbalancedTx)
import           Ledger.Tx                              (CardanoTx (..), SomeCardanoApiTx (..))
import           Ouroboros.Consensus.Shelley.Eras       (StandardAlonzo, StandardBabbage, StandardShelley)
import           Plutus.V1.Ledger.Bytes                 (bytes, fromBytes)
import           Plutus.V2.Ledger.Api                   (fromBuiltin, toBuiltin)
import           PlutusAppsExtra.Types.Error            (MkTxError (..))
import           Text.Hex                               (decodeHex)
import qualified Data.Map as Map

------------------------ Export/Import of transactions -------------------------

unbalancedTxToCBOR :: Params -> UnbalancedTx -> Maybe Text
unbalancedTxToCBOR params =
    fmap (encodeByteString . serialiseToCBOR . partialTx) . eitherToMaybe . export params

textToCardanoTx :: Text -> Maybe CardanoTx
textToCardanoTx txt = do
    bs <- eitherToMaybe $ tryDecode txt
    tx <- eitherToMaybe $ deserialiseFromCBOR (AsTx AsBabbageEra) bs
    return $ CardanoApiTx $ SomeTx tx BabbageEraInCardanoMode

cardanoTxToText :: CardanoTx -> Maybe Text
cardanoTxToText (CardanoApiTx (SomeTx tx BabbageEraInCardanoMode)) = Just $ encodeByteString $ serialiseToCBOR tx
cardanoTxToText _ = Nothing

apiSerializedTxToCardanoTx :: ApiSerialisedTransaction -> Maybe CardanoTx
apiSerializedTxToCardanoTx = fmap CardanoApiTx . toSomeTx . toAnyEraTx
    where
        toAnyEraTx = cardanoTxIdeallyNoLaterThan (AnyCardanoEra BabbageEra) . getApiT . transaction
        toSomeTx (InAnyCardanoEra cera tx) = SomeTx tx <$> toEraInMode cera CardanoMode

cardanoTxToSealedTx :: CardanoTx -> Maybe SealedTx
cardanoTxToSealedTx = \case
    (CardanoApiTx (SomeTx tx _)) -> Just $ sealedTxFromCardano' tx
    _                            -> Nothing

addMetadataToCardanoTx :: (MonadThrow m, ToJSON a) => CardanoTx -> Maybe a -> TxMetadataJsonSchema -> m CardanoTx
addMetadataToCardanoTx ctx Nothing _ = pure ctx 
addMetadataToCardanoTx ctx (Just val) schema = case metadataFromJson schema (toJSON val) of
    Left _     -> throwM UnparsableMetadata
    Right meta -> pure $ addMetadataToCardanoTx' ctx meta
    
addMetadataToCardanoTx'  :: CardanoTx -> C.TxMetadata -> CardanoTx
addMetadataToCardanoTx' ctx (C.TxMetadata meta) = cardanoTxMap onTx onSomeTx ctx
    where
        metadataBs = serialiseToCBOR $ C.TxMetadata meta
        metadataShelley = maybeToStrictMaybe $ Just $ Shelley.Metadata $ toShelleyMetadata meta
        metadataAllegra mb = mb <&> \(Allegra.AuxiliaryData a s) -> Allegra.AuxiliaryData a s
        metadataAlonzo  mb = mb <&> \Alonzo.AuxiliaryData{..} -> Alonzo.AuxiliaryData{txMD = toShelleyMetadata meta, ..}
        metadataBabbage mb = mb <&> \Alonzo.AuxiliaryData{..} -> Alonzo.AuxiliaryData{txMD = toShelleyMetadata meta, ..}

        onTx tx = tx { txMetadata = Just $ toBuiltin metadataBs}

        onSomeTx (SomeTx (Byron.ByronTx (ATxAux tx wit _)) era)
            = SomeTx (Byron.ByronTx (ATxAux tx wit metadataBs)) era
        onSomeTx (SomeTx (ShelleyTx sera tx) era) = (`SomeTx` era) $ case sera of
            C.ShelleyBasedEraShelley -> ShelleyTx C.ShelleyBasedEraShelley (addShelley tx)
            C.ShelleyBasedEraAllegra -> ShelleyTx C.ShelleyBasedEraAllegra (addAllegra tx)
            C.ShelleyBasedEraMary    -> ShelleyTx C.ShelleyBasedEraMary    (addAllegra tx)
            C.ShelleyBasedEraAlonzo  -> ShelleyTx C.ShelleyBasedEraAlonzo  (addAlonzo  tx)
            C.ShelleyBasedEraBabbage -> ShelleyTx C.ShelleyBasedEraBabbage (addBabbage tx)

        addShelley :: Shelley.Tx StandardShelley -> Shelley.Tx StandardShelley
        addShelley Shelley.Tx{..} = Shelley.Tx{auxiliaryData = metadataShelley, ..}

        addAllegra Shelley.Tx{..} = Shelley.Tx{auxiliaryData = metadataAllegra auxiliaryData, ..}

        addAlonzo :: ValidatedTx StandardAlonzo -> ValidatedTx StandardAlonzo
        addAlonzo ValidatedTx{..} = ValidatedTx{Alonzo.auxiliaryData = metadataAlonzo auxiliaryData, ..}

        addBabbage :: ValidatedTx StandardBabbage -> ValidatedTx StandardBabbage
        addBabbage ValidatedTx{..} = ValidatedTx{Alonzo.auxiliaryData = metadataBabbage auxiliaryData, ..}

------------------------ External keys and signatures -------------------------

textToPubkey :: Text -> Maybe PubKey
textToPubkey txt = PubKey . fromBytes <$> decodeHex txt

textToSignature :: Text -> Maybe Signature
textToSignature txt = Signature . toBuiltin <$> decodeHex txt

addCardanoTxSignature :: PubKey -> Signature -> CardanoTx -> CardanoTx
addCardanoTxSignature pubKey sig = cardanoTxMap addSignatureTx addSignatureCardano
    where
        addSignatureTx tx = tx & signatures . at pubKey ?~ sig

        addSignatureCardano (CardanoApiEmulatorEraTx ctx)
            = CardanoApiEmulatorEraTx (addSignatureCardano' ctx)

        addSignatureCardano' (ShelleyTx shelleyBasedEra (ValidatedTx body wits isValid aux)) =
            let wits' = wits <> mempty { Wits.txwitsVKey = Set.singleton $ WitVKey (VKey vk) sig' }
            in  ShelleyTx shelleyBasedEra (ValidatedTx body wits' isValid aux)

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