{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TupleSections              #-}

module PlutusAppsExtra.Utils.Kupo where

import           Cardano.Api                   (TxIx (..), NetworkId (Mainnet))
import           Cardano.Chain.Block           ( HeaderHash )
import           Codec.Serialise               (deserialise)
import           Data.Aeson                    (FromJSON (..), withObject, (.:), ToJSON, KeyValue ((.=)))
import qualified Data.Aeson                    as J
import qualified Data.Aeson.Key 
import qualified Data.Aeson.Key                as J
import qualified Data.Aeson.KeyMap             as J
import           Data.Bifunctor                (Bifunctor(..))
import qualified Data.ByteString.Lazy          as LBS
import           Data.Coerce                   (coerce)
import           Data.Fixed                    (Fixed(..))
import           Data.Functor                  ((<&>))
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)
import           Ledger                        (Address (..), Datum (..), DatumFromQuery (..), DatumHash (..), Language (..),
                                                PubKeyHash, Script, ScriptHash (..), TxId (..), TxOutRef (..), Validator (..),
                                                ValidatorHash (..), Value, Versioned (..), Slot (Slot), CurrencySymbol, TokenName, noAdaValue)
import qualified Ledger.Ada                    as Ada
import qualified Ledger.Value                  as Value
import           Plutus.V1.Ledger.Api          (Credential (..), StakingCredential (..), fromBuiltin, toBuiltin, CurrencySymbol (..))
import           PlutusAppsExtra.Utils.Address (bech32ToAddress, addressToBech32)
import qualified PlutusTx.AssocMap             as PMap
import           PlutusTx.Builtins             (BuiltinByteString)
import           Servant.API                   (ToHttpApiData (..))
import           Text.Hex                      (decodeHex, encodeHex)
import           Ledger.Value                  (flattenValue, TokenName (..))
------------------------------------------- Newtype to avoid orphans -------------------------------------------

newtype Kupo a = Kupo a

type KupoUTXO  = Kupo (TxOutRef, KupoDecoratedTxOut)
type KupoUTXOs = [KupoUTXO]

type KupoAddress = (Either KupoWildCard Credential, Either KupoWildCard StakingCredential)

mkKupoAddress :: Address -> KupoAddress
mkKupoAddress Address{..} = (Right addressCredential, maybe (Left KupoWildCard) Right addressStakingCredential)

mkAddressWithAnyCred :: StakingCredential -> KupoAddress
mkAddressWithAnyCred = (Left KupoWildCard ,) . Right

addressWithAnySCred :: Credential -> KupoAddress
addressWithAnySCred cred = first (const $ Right cred) anyAddress

anyAddress :: KupoAddress
anyAddress = (Left KupoWildCard, Left KupoWildCard)

type KupoAsset = (CurrencySymbol, Either KupoWildCard TokenName)

data Pattern = AddrPattern KupoAddress | AssetPattern KupoAsset | WildCardPattern

data KupoDecoratedTxOut
    = KupoPublicKeyDecoratedTxOut
        { _decoratedTxOutPubKeyHash          :: PubKeyHash
        , _decoratedTxOutStakingCredential   :: Maybe StakingCredential
        , _decoratedTxOutValue               :: Value
        , _decoratedTxOutPubKeyDatumHash     :: Maybe (DatumHash, Datum -> DatumFromQuery)
        , _decoratedTxOutReferenceScriptHash :: Maybe ScriptHash
        }
    | KupoScriptDecoratedTxOut
        { _decoratedTxOutValidatorHash       :: ValidatorHash
        , _decoratedTxOutStakingCredential   :: Maybe StakingCredential
        , _decoratedTxOutValue               :: Value
        , _decoratedTxOutScriptDatum         :: (DatumHash, Datum -> DatumFromQuery)
        , _decoratedTxOutReferenceScriptHash :: Maybe ScriptHash
        }

----------------------------------------------- FromJSON instances -----------------------------------------------

data KupoResponse = KupoResponse
    { krTransactionId :: TxId
    , krOutputIndex :: TxIx
    , krAddress :: Address
    , krValue :: Value
    , krDatumHash :: Maybe DatumHash
    -- , krDatumType :: DatumFromQuery
    , krScriptHash :: Maybe ScriptHash
    , krCreatedAt :: SlotWithHeaderHash
    , krSpentAt :: Maybe SlotWithHeaderHash
    } deriving (Show, Generic)

data SlotWithHeaderHash = SlotWithHeaderHash
    { swhhSlot :: Slot
    , swhhHeaderHash :: HeaderHash
    } deriving (Show, Generic)

instance FromJSON KupoResponse where
    parseJSON = withObject "KupoResponse" $ \o -> do
        krTransactionId  <- o .: "transaction_id" <&> TxId
        krOutputIndex <- o .: "output_index"
        krAddress <-  o .: "address" >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
        Kupo krValue <- o .: "value"
        krDatumHash <- o .: "datum_hash" >>= \case
            J.Null -> pure Nothing
            J.String dHash -> Just . DatumHash <$> toBbs dHash
            val -> fail $ show val
        krScriptHash <- o .: "script_hash" >>= \case
            J.Null -> pure Nothing
            J.String sHash -> Just . ScriptHash <$> toBbs sHash
            val -> fail $ show val
        krCreatedAt <- o .: "created_at"
        krSpentAt <- o .: "spent_at"
        pure KupoResponse{..}
        where toBbs = maybe (fail "not a hex") (pure . toBuiltin) . decodeHex

instance ToJSON KupoResponse where
    toJSON KupoResponse{..} = J.object
        ["transaction_id" .= getTxId krTransactionId
        , "output_index" .= krOutputIndex
        , "address" .= addressToBech32 Mainnet krAddress
        , "value" .= J.Object
            [ "coins" .= (\(MkFixed i) -> i) (Ada.getAda (Ada.fromValue krValue))
            , "assets" .= J.fromList (map (\(cs, tn, amt) -> 
                (Data.Aeson.Key.fromText $ encodeHex $ fromBuiltin $ unCurrencySymbol cs <> unTokenName tn, amt)) $ flattenValue (noAdaValue krValue))
            ]
        , "datum_hash" .= krDatumHash
        , "script_hash" .= krScriptHash
        , "created_at" .= krCreatedAt
        , "spent_at" .= krSpentAt
        ]

instance FromJSON SlotWithHeaderHash where
    parseJSON = withObject "KupoResponse" $ \o -> do
        swhhSlot <- fromInteger <$> o .: "slot_no"
        swhhHeaderHash <- o .: "header_hash"
        pure SlotWithHeaderHash{..}

instance ToJSON SlotWithHeaderHash where
    toJSON SlotWithHeaderHash{..} = J.object
        ["slot_no" .= toInteger swhhSlot
        , "header_hash" .= swhhHeaderHash
        ]

instance {-# OVERLAPPING #-} FromJSON (Maybe KupoUTXO) where
    parseJSON j = ($ j) $ withObject "Kupo UTXO" $ \o -> do
        refId  <- o .: "transaction_id" <&> TxId
        refIdX <- o .: "output_index"
        parseJSON j >>= \case
            Just txOut -> pure $ Just $ Kupo (TxOutRef refId refIdX, txOut)
            _          -> pure Nothing

-- instance FromJSON KupoUTXO where
--     parseJSON j = ($ j) $ withObject "Kupo UTXO" $ \o -> do
--         refId  <- o .: "transaction_id" <&> TxId
--         refIdX <- o .: "output_index"
--         txOut <- parseJSON j
--         pure $ Kupo (TxOutRef refId refIdX, txOut)

instance {-# OVERLAPPING #-} FromJSON (Maybe KupoDecoratedTxOut) where
    parseJSON = withObject "KupoDecoratedTxOut" $ \o -> do
        addr      <- (o .: "address") >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
        Kupo val  <- o .: "value"
        datumHash <- o .: "datum_hash" >>= \case
            J.Null        -> pure Nothing
            J.String hash -> Just . DatumHash <$> toBbs hash
            _             -> fail "datum hash"
        datum <- case datumHash of
            Nothing -> pure Nothing
            Just dh  -> o .: "datum_type" >>= \case
                J.String "hash"   -> pure $ Just (dh, DatumInBody)
                J.String "inline" -> pure $ Just (dh, DatumInline)
                _                 -> fail "datum type"
        let sc = addressStakingCredential addr
        script    <- o .: "script_hash" >>= \case
            J.Null      -> pure Nothing
            J.String sh -> Just . ScriptHash <$> toBbs sh
            _           -> fail "script hash"
        case addressCredential addr of
            PubKeyCredential pkh -> pure $ Just $ KupoPublicKeyDecoratedTxOut pkh sc val datum script
            ScriptCredential vh -> case datum of
                Just datum' -> pure $ Just $ KupoScriptDecoratedTxOut vh sc val datum' script
                _           -> pure Nothing
        where toBbs = maybe (fail "not a hex") (pure . toBuiltin) . decodeHex

-- instance FromJSON KupoDecoratedTxOut where
--     parseJSON = withObject "KupoDecoratedTxOut" $ \o -> do
--         addr      <- (o .: "address") >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
--         Kupo val  <- o .: "value"
--         datumHash <- o .: "datum_hash" >>= \case
--             J.Null        -> pure Nothing
--             J.String hash -> Just . DatumHash <$> toBbs hash
--             _             -> fail "datum hash"
--         datum <- case datumHash of
--             Nothing -> pure Nothing
--             Just dh  -> o .: "datum_type" >>= \case
--                 J.String "hash"   -> pure $ Just (dh, DatumInBody)
--                 J.String "inline" -> pure $ Just (dh, DatumInline)
--                 _                 -> fail "datum type"
--         let sc = addressStakingCredential addr
--         script    <- o .: "script_hash" >>= \case
--             J.Null      -> pure Nothing
--             J.String sh -> Just . ScriptHash <$> toBbs sh
--             _           -> fail "script hash"
--         case addressCredential addr of
--             PubKeyCredential pkh -> pure $ KupoPublicKeyDecoratedTxOut pkh sc val datum script
--             ScriptCredential vh -> do
--                 datum' <- maybe (fail "script txOut without datum") pure datum
--                 pure $ KupoScriptDecoratedTxOut vh sc val datum' script
--         where toBbs = maybe (fail "not a hex") (pure . toBuiltin) . decodeHex

instance FromJSON (Kupo (Versioned Script)) where
    parseJSON = withObject "Kupo Versioned Script" $ \o -> do
        script <- o .: "script"
        lang   <- o .: "language" >>= \case
            J.String "plutus:v1" -> pure PlutusV1
            J.String "plutus:v2" -> pure PlutusV2
            _                    -> fail "script language"
        pure $ Kupo $ Versioned script lang

instance FromJSON (Kupo (Versioned Validator)) where
    parseJSON = withObject "Kupo Versioned Validator" $ \o -> do
        validator <- Validator <$> o .: "script"
        lang   <- o .: "language" >>= \case
            J.String "plutus:v1" -> pure PlutusV1
            J.String "plutus:v2" -> pure PlutusV2
            _                    -> fail "script language"
        pure $ Kupo $ Versioned validator lang

instance FromJSON (Kupo Datum) where
    parseJSON = withObject "Kupo Datum" $ \o -> o .: "datum" >>=
        maybe (fail "not a hex") (pure . Kupo . Datum . deserialise . LBS.fromStrict) . decodeHex

instance FromJSON (Kupo Value) where
    parseJSON = withObject "Kupo Value" $ \o -> do
            coins <-  o .: "coins" <&> Ada.toValue . Ada.lovelaceOf
            assets <- o .: "assets" >>= parseAssets
            pure $ Kupo $ coins <> assets
        where
            parseAssets = fmap mconcat . traverse parseAsset . J.toList
            parseAsset (asset, amount) = Value.Value . PMap.fromList <$> do
                amount' <- parseJSON amount
                case span (/= '.') (J.toString asset) of
                    (cs, '.' : name) -> do
                        name'   <- Value.TokenName      <$> toBbs name
                        cs'     <- Value.CurrencySymbol <$> toBbs cs
                        pure [(cs', PMap.singleton name' amount')]
                    (cs, _) -> do
                        cs'     <- Value.CurrencySymbol <$> toBbs cs
                        pure [(cs', PMap.singleton "" amount')]
            toBbs = maybe (fail "not a hex") (pure . toBuiltin) . decodeHex . T.pack

-------------------------------------------- ToHttpApiData instances --------------------------------------------

instance ToHttpApiData (Kupo TxOutRef) where
    toUrlPiece (Kupo TxOutRef{..}) = T.pack (show txOutRefIdx <> "@") <>
        encodeHex (fromBuiltin $ getTxId txOutRefId)

instance ToHttpApiData Pattern where
    toUrlPiece = \case
        AddrPattern addr   -> toUrlPiece addr
        AssetPattern asset -> toUrlPiece asset
        WildCardPattern    -> "*"

instance ToHttpApiData KupoAddress where
    toUrlPiece (cred, sCred) = T.pack $ showCred cred <> "/" <> showSCred sCred
        where
            showCred = \case
                Right (PubKeyCredential c) -> show c
                Right (ScriptCredential c) -> show c
                Left _ -> "*"
            showSCred = \case
                Right (StakingHash sHash) -> showCred $ Right sHash
                _                         -> "*"

instance ToHttpApiData KupoAsset where
    toUrlPiece (cs, name) = toUrlPiece (Kupo cs) <> "." <> either toUrlPiece (toUrlPiece . Kupo) name

deriving via (Kupo ScriptHash) instance ToHttpApiData (Kupo CurrencySymbol)
deriving via (Kupo ScriptHash) instance ToHttpApiData (Kupo TokenName)

instance ToHttpApiData (Kupo ScriptHash) where
    toUrlPiece = encodeHex . fromBuiltin @BuiltinByteString . coerce

deriving via (Kupo ScriptHash) instance ToHttpApiData (Kupo ValidatorHash)
deriving via (Kupo ScriptHash) instance ToHttpApiData (Kupo DatumHash)

data KupoWildCard = KupoWildCard

instance ToHttpApiData KupoWildCard where
    toUrlPiece _ = "*"

instance ToHttpApiData (Kupo Slot) where
    toUrlPiece (Kupo (Slot i)) = T.pack $ show i