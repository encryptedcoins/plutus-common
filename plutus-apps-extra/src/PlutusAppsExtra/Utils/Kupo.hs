{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}

module PlutusAppsExtra.Utils.Kupo where

import qualified Cardano.Api                   as C
import           Cardano.Chain.Block           (HeaderHash)
import           Codec.Serialise               (deserialise)
import           Data.Aeson                    (FromJSON (..), KeyValue ((.=)), ToJSON, withObject, (.:), (.:?))
import qualified Data.Aeson                    as J
import qualified Data.Aeson.Key
import qualified Data.Aeson.Key                as J
import qualified Data.Aeson.KeyMap             as J
import qualified Data.ByteString.Lazy          as LBS
import           Data.Coerce                   (coerce)
import           Data.Functor                  ((<&>))
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)
import           Ledger                        (Address (..), Datum (..), DatumFromQuery (..), DatumHash (..), Language (..),
                                                NetworkId, Script, ScriptHash (..), Slot (Slot), TxId (..), TxOutRef (..),
                                                Validator (..), ValidatorHash (..), Versioned (..), fromCardanoValue,
                                                toCardanoValue, adaOnlyValue)
import           Plutus.Script.Utils.Ada       (lovelaceOf, toValue)
import           Plutus.Script.Utils.Value     (AssetClass (..))
import           Plutus.V1.Ledger.Api          (Credential (..), CurrencySymbol (..), StakingCredential (..), TokenName,
                                                fromBuiltin, toBuiltin)
import           Plutus.V1.Ledger.Value        (flattenValue)
import           Plutus.V2.Ledger.Api          (TokenName (..), Value (..))
import           PlutusAppsExtra.Utils.Address (addressToBech32, bech32ToAddress)
import qualified PlutusTx.AssocMap             as PMap
import           PlutusTx.Builtins             (BuiltinByteString)
import           Servant.API                   (ToHttpApiData (..))
import           Text.Hex                      (decodeHex, encodeHex)

------------------------------------- Newtype to avoid orphans and instance clashes -------------------------------------

newtype Kupo a = Kupo a

----------------------------------------------------- Kupo patterns -----------------------------------------------------

class MkPattern a where
    mkPattern :: a -> Pattern

data Pattern
    = AddrPattern KupoAddress
    | AssetPattern KupoAsset
    | TxOutRefPattern KupoTxOutRef
    | WildCardPattern
    deriving (Show, Eq)

instance MkPattern Pattern where
    mkPattern = id

data KupoAddress = KupoAddress (Maybe Credential) (Maybe StakingCredential)
    deriving (Show, Eq)

instance MkPattern Address where
    mkPattern Address{..} = AddrPattern $ KupoAddress (Just addressCredential) addressStakingCredential

instance MkPattern Credential where
    mkPattern cred = AddrPattern $ KupoAddress (Just cred) Nothing

instance MkPattern StakingCredential where
    mkPattern sCred = AddrPattern $ KupoAddress Nothing (Just sCred)

anyAddress :: Pattern
anyAddress = AddrPattern $ KupoAddress Nothing Nothing

data KupoAsset = KupoAsset CurrencySymbol (Maybe TokenName)
    deriving (Show, Eq)

instance MkPattern CurrencySymbol where
    mkPattern cs = AssetPattern $ KupoAsset cs Nothing

instance MkPattern AssetClass where
    mkPattern = AssetPattern . uncurry KupoAsset . fmap Just . unAssetClass

data KupoTxOutRef = KupoTxOutRef TxId (Maybe Integer)
    deriving (Show, Eq)

instance MkPattern TxId where
    mkPattern = TxOutRefPattern . (`KupoTxOutRef` Nothing)

instance MkPattern TxOutRef where
    mkPattern TxOutRef{..} = TxOutRefPattern $ KupoTxOutRef txOutRefId $ Just txOutRefIdx

----------------------------------------------- FromJSON instances -----------------------------------------------

data KupoResponse = KupoResponse
    { krTxId        :: !TxId
    , krOutputIndex :: !Integer
    , krAddress     :: !Address
    , krValue       :: !C.Value
    , krDatumHash   :: !(Maybe DatumHash)
    , krDatumType   :: !(Maybe KupoDatumType)
    , krScriptHash  :: !(Maybe ScriptHash)
    , krCreatedAt   :: !SlotWithHeaderHash
    , krSpentAt     :: !(Maybe SlotWithHeaderHash)
    } deriving (Show, Eq, Generic)

data KupoDatumType = KupoDatumHash | KupoDatumInline
    deriving (Show , Eq)

fromKupoDatumType :: KupoDatumType -> (Datum -> DatumFromQuery)
fromKupoDatumType = \case
    KupoDatumHash   -> DatumInBody
    KupoDatumInline -> DatumInline

data SlotWithHeaderHash = SlotWithHeaderHash
    { swhhSlot :: Slot
    , swhhHeaderHash :: HeaderHash
    } deriving (Show, Eq, Generic)

instance FromJSON KupoResponse where
    parseJSON = withObject "KupoResponse" $ \o -> do
        krTxId          <- o .: "transaction_id" <&> TxId
        krOutputIndex   <- o .: "output_index"
        krAddress       <- o .: "address" >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
        Kupo krValue    <- o .: "value"
        krCreatedAt     <- o .: "created_at"
        krSpentAt       <- o .: "spent_at"
        krDatumHash     <- o .: "datum_hash" >>= \case
            J.Null -> pure Nothing
            J.String dHash -> Just . DatumHash <$> toBbs dHash
            val -> fail $ show val
        krDatumType <- o .:? "datum_type" >>= \case
            Just (J.String "hash"  ) -> pure $ Just KupoDatumHash
            Just (J.String "inline") -> pure $ Just KupoDatumInline
            Nothing                  -> pure   Nothing
            Just val                 -> fail $ show val
        krScriptHash <- o .: "script_hash" >>= \case
            J.Null -> pure Nothing
            J.String sHash -> Just . ScriptHash <$> toBbs sHash
            val -> fail $ show val
        pure KupoResponse{..}
        where toBbs = maybe (fail "not a hex") (pure . toBuiltin) . decodeHex

kupoResponseToJSON :: NetworkId -> KupoResponse -> J.Value
kupoResponseToJSON networkId KupoResponse{..} = J.object
    ["transaction_id" .= getTxId krTxId
    , "output_index"  .= krOutputIndex
    , "address"       .= addressToBech32 networkId krAddress
    , "datum_hash"    .= krDatumHash
    , "script_hash"   .= krScriptHash
    , "created_at"    .= krCreatedAt
    , "spent_at"      .= krSpentAt
    , "value"         .= J.Object
        [ "coins"     .= toInteger (C.selectLovelace krValue)
        , "assets"    .= J.fromList (flip fmap (flattenValue (fromCardanoValue krValue))
            $ \(cs, tn, amt) -> (Data.Aeson.Key.fromText $ encodeHex $ fromBuiltin $ unCurrencySymbol cs <> unTokenName tn, amt))
        ]
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

instance FromJSON (Kupo C.Value) where
    parseJSON = withObject "Kupo Value" $ \o -> do
            coins <-  o .: "coins" <&> toValue . lovelaceOf
            assets <- o .: "assets" >>= parseAssets
            either (fail . show) (pure . Kupo) $ toCardanoValue $ coins <> assets
        where
            parseAssets = fmap mconcat . traverse parseAsset . J.toList
            parseAsset (asset, amount) = Value . PMap.fromList <$> do
                amount' <- parseJSON amount
                case span (/= '.') (J.toString asset) of
                    (cs, '.' : name) -> do
                        name'   <- TokenName      <$> toBbs name
                        cs'     <- CurrencySymbol <$> toBbs cs
                        pure [(cs', PMap.singleton name' amount')]
                    (cs, _) -> do
                        cs'     <- CurrencySymbol <$> toBbs cs
                        pure [(cs', PMap.singleton "" amount')]
            toBbs = maybe (fail "not a hex") (pure . toBuiltin) . decodeHex . T.pack

data GetHealthResponse = GetHealthResponse
    { ghrConnected            :: Bool
    , ghrMostRecentCheckpoint :: Integer
    , ghrMostRecentNodeTip    :: Integer
    , ghrVersion              :: String
    } deriving (Show, Generic)

instance FromJSON GetHealthResponse where
    parseJSON = withObject "GetHealthResponse" $ \o -> do
        ghrConnected <- o .: "connection_status" >>= \case
            J.String "connected"    -> pure True
            J.String "disconnected" -> pure False
            val                     -> fail $ show val
        ghrMostRecentCheckpoint <- o .: "most_recent_checkpoint"
        ghrMostRecentNodeTip    <- o .: "most_recent_node_tip"
        ghrVersion              <- o .: "version"
        pure GetHealthResponse{..}


filterCleanKupoResponses :: [KupoResponse] -> [KupoResponse]
filterCleanKupoResponses = filter $ (\v -> adaOnlyValue v == v) . krValue

-------------------------------------------- ToHttpApiData instances --------------------------------------------

instance ToHttpApiData Pattern where
    toUrlPiece = \case
        AddrPattern (KupoAddress cred sCred) -> T.pack $ showCred cred <> "/" <> showSCred sCred
        AssetPattern (KupoAsset cs name) -> toUrlPiece (Kupo cs) <> "." <> maybe "*" (toUrlPiece . Kupo) name
        TxOutRefPattern (KupoTxOutRef txId txIdx) -> maybe "*" toUrlPiece txIdx <> "@" <> toUrlPiece (Kupo txId)
        WildCardPattern -> "*"
        where
            showCred = \case
                Just (PubKeyCredential c) -> show c
                Just (ScriptCredential c) -> show c
                Nothing -> "*"
            showSCred = \case
                Just (StakingHash sHash) -> showCred $ Just sHash
                _                         -> "*"

deriving via (Kupo ScriptHash) instance ToHttpApiData (Kupo CurrencySymbol)
deriving via (Kupo ScriptHash) instance ToHttpApiData (Kupo TokenName)
deriving via (Kupo ScriptHash) instance ToHttpApiData (Kupo TxId)

instance ToHttpApiData (Kupo ScriptHash) where
    toUrlPiece = encodeHex . fromBuiltin @BuiltinByteString . coerce

deriving via (Kupo ScriptHash) instance ToHttpApiData (Kupo ValidatorHash)
deriving via (Kupo ScriptHash) instance ToHttpApiData (Kupo DatumHash)

instance ToHttpApiData (Kupo Slot) where
    toUrlPiece (Kupo (Slot i)) = T.pack $ show i

data KupoOrder = KupoOrderAsc | KupoOrerDesc
    deriving (Show, Eq)

instance ToHttpApiData KupoOrder where
    toUrlPiece = \case
        KupoOrderAsc -> "oldest_first"
        KupoOrerDesc -> "most_recent_first"
