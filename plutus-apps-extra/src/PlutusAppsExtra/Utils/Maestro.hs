{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

module PlutusAppsExtra.Utils.Maestro where

import qualified Cardano.Api                    as C
import           Control.Monad                  (forM, mzero)
import           Data.Aeson                     (FromJSON (..), withObject, (.:), (.:?))
import qualified Data.Aeson                     as J
import           Data.Coerce                    (coerce)
import           Data.Functor                   ((<&>))
import           Data.Maybe                     (catMaybes)
import           Data.Scientific                (floatingOrInteger)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Time                      as Time
import           GHC.Generics                   (Generic)
import           GHC.Records                    (HasField (..))
import           Ledger                         (Address, Datum (..), DatumFromQuery (..), DatumHash, Language (..), PubKeyHash (..),
                                                 Script, ScriptHash (..), Slot (..), StakePubKeyHash (..), TxId (..), Versioned (..))
import           Ledger.Scripts                 (Script (..))
import           Plutus.Script.Utils.Value      (AssetClass (..))
import           PlutusAppsExtra.IO.Tx.Internal (TxState)
import           PlutusAppsExtra.Utils.Address  (bech32ToAddress, bech32ToStakePubKeyHash)
import           PlutusAppsExtra.Utils.Scripts  (scriptFromCBOR)
import qualified PlutusLedgerApi.V1             as PV1
import           PlutusLedgerApi.V3             (BuiltinByteString, CurrencySymbol (..), TokenName (..), fromBuiltin, toBuiltin)
import           Servant.API                    (ToHttpApiData (..))
import qualified Text.Hex                       as T
import           Text.Hex                       (decodeHex, encodeHex)
import           Text.Read                      (readMaybe)

newtype Maestro a = Maestro {unMaestro :: a}

deriving newtype instance Show a => Show (Maestro a)

type Cursor = String

type HasCursor a = HasField "cursor" a (Maybe Cursor)

getCursor :: HasCursor a => a -> Maybe Cursor
getCursor = getField @"cursor"

data AccountAddressesHoldingAssetsResponse = AccountAddressesHoldingAssetsResponse
    { aaharData   :: [(PubKeyHash, Integer)]
    , aaharCursor :: Maybe Cursor
    } deriving (Show)

instance FromJSON AccountAddressesHoldingAssetsResponse where
    parseJSON = withObject "TxDetailsResponse" $ \o -> do
        aaharCursor <- o .:? "next_cursor"
        d           <- o .:  "data"
        aaharData   <- fmap catMaybes $ forM d $ \a -> do
            stakeAddr <- a .: "account" <&> fmap unStakePubKeyHash . bech32ToStakePubKeyHash
            amount    <- a .: "amount"
            pure $ (, amount) <$> stakeAddr
        pure AccountAddressesHoldingAssetsResponse{..}

instance HasField "cursor" AccountAddressesHoldingAssetsResponse (Maybe Cursor) where
    getField = aaharCursor

data AssetMintsAndBurnsResponse = AssetMintsAndBurnsResponse
    { ambrData   :: [AssetMintsAndBurnsData]
    , ambrCursor :: Maybe Cursor
    } deriving (Show)

instance HasField "cursor" AssetMintsAndBurnsResponse (Maybe Cursor) where
    getField = ambrCursor

data AssetMintsAndBurnsData = AssetMintsAndBurnsData
    { ambrAmount    :: Integer
    , ambrSlot      :: Slot
    , ambrTimestamp :: Time.UTCTime
    , ambrTxHash    :: TxId
    } deriving (Show)

instance FromJSON AssetMintsAndBurnsResponse where
    parseJSON = withObject "AssetMintsAndBurnsResponse" $ \o -> do
        ambrCursor    <- o .:? "next_cursor"
        ambrData      <- o .: "data"
        pure AssetMintsAndBurnsResponse{..}

instance FromJSON AssetMintsAndBurnsData where
    parseJSON = withObject "AssetMintsAndBurnsData" $ \o -> do
        ambrAmount    <- o .: "amount" >>= maybe (fail "AssetMintsAndBurnsResponse: read amount") pure . readMaybe
        ambrSlot      <- o .: "slot" <&> Slot
        ambrTimestamp <- o .: "timestamp" >>= Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d %H:%M:%S"
        ambrTxHash    <- o .: "tx_hash" <&> TxId
        pure AssetMintsAndBurnsData{..}

data ScriptByHashResponse = ScriptByHashResponse
    { sbhrScript     :: Versioned Script
    , sbhrScriptHash :: ScriptHash
    } deriving (Show, Eq)

instance FromJSON ScriptByHashResponse where
    parseJSON = withObject "ScriptByHashResponse" $ \o -> do
        d                  <- o .: "data"
        Maestro sbhrScript <- o .: "data"
        sbhrScriptHash     <- d .: "hash"
            >>= J.withText "script hash" (fmap (coerce . toBuiltin) . maybe (fail "decodeHex") pure . decodeHex)
        pure ScriptByHashResponse{..}

data TxDetailsResponse = TxDetailsResponse
    { tdrTxHash            :: TxId
    , tdrOutputs           :: [TxDetailsOutput]
    , tdrAdditionalSigners :: [PubKeyHash]
    , tdrSlot              :: Slot
    } deriving (Show)

instance FromJSON TxDetailsResponse where
    parseJSON = withObject "TxDetailsResponse" $ \o -> do
        d                    <- o .: "data"
        tdrTxHash            <- d .: "tx_hash" <&> TxId
        tdrOutputs           <- d .: "outputs"
        tdrSlot              <- d .:  "block_absolute_slot" <&> fromInteger
        tdrAdditionalSigners <- d .: "additional_signers" >>=
            mapM (maybe (fail "additional_signers decodeHex") (pure . PubKeyHash . toBuiltin) . decodeHex)
        pure TxDetailsResponse{..}

data TxDetailsOutput = TxDetailsOutput
    { tdoTxHash  :: TxId
    , tdoIndex   :: Integer
    , tdoAddress :: Address
    , tdoDatum   :: Maybe (DatumHash, DatumFromQuery)
    } deriving (Show)

instance FromJSON TxDetailsOutput where
    parseJSON = withObject "TxDetailsOutput" $ \o -> do
        tdoTxHash  <- o .:  "tx_hash" <&> TxId
        tdoIndex   <- o .:  "index"
        tdoAddress <- o .:  "address" >>= maybe (fail "bech32ToAddress TxDetailsOutput") pure . bech32ToAddress
        tdoDatum   <- o .:? "datum" <&> fmap unMaestro
        pure TxDetailsOutput{..}

data TxOutputResponse = TxOutputResponse
    { torAddress         :: Address
    , torValue           :: C.Value
    , torDatum           :: Maybe (DatumHash, DatumFromQuery)
    , torReferenceScript :: Maybe (Versioned Script)
    } deriving (Show, Eq)

instance FromJSON TxOutputResponse where
    parseJSON = withObject "TxOutputResponse" $ \o -> do
        d                  <- o .: "data"
        torAddress         <- d .: "address" >>= maybe (fail "bech32ToAddress TxDetailsOutput") pure . bech32ToAddress
        torValue           <- d .: "assets" <&> mconcat . fmap unMaestro
        torDatum           <- d .:? "datum" <&> fmap unMaestro
        torReferenceScript <- o .:? "reference_script" <&> fmap unMaestro
        pure TxOutputResponse{..}

newtype TxHistoryResponse = TxHistoryResponse {txsHistoryState :: [TxStateResponse]}
    deriving (Show, Eq, Generic)
    deriving newtype FromJSON

data TxStateResponse = TxStateResponse
    { tsrBlock     :: Maybe Int
    , tsrState     :: TxState
    , tsrTimestamp :: Time.UTCTime
    , tsrTxHash    :: PV1.TxId
    } deriving (Show, Eq)

instance FromJSON TxStateResponse where
    parseJSON = withObject "TxHistoryData" $ \o -> do
        tsrBlock     <- (o .: "block" >>=) $ J.withText "block" $ \case
            "-" -> pure Nothing
            num -> maybe (fail "read block") (pure . Just) $ readMaybe $ T.unpack num
        tsrState     <- (o .:  "state" >>=) $ J.withText "state" $ maybe (fail "read txState") pure . readMaybe . T.unpack
        tsrTimestamp <- o .: "timestamp"
        tsrTxHash    <- o .: "transaction_hash"  <&> PV1.TxId
        pure TxStateResponse{..}

data UtxosAtAddressResponse = UtxosAtAddressResponse
    { uaarData   :: [UtxosAtAddressData]
    , uaarCursor :: Maybe Cursor
    } deriving (Show, Eq)

instance HasField "cursor" UtxosAtAddressResponse (Maybe Cursor) where
    getField = uaarCursor

data UtxosAtAddressData = UtxosAtAddressData
    { uaadTxHash          :: PV1.TxId
    , uaadIndex           :: Integer
    , uaadAddress         :: Address
    , uaadValue           :: C.Value
    , uaadDatum           :: Maybe (DatumHash, DatumFromQuery)
    , uaadReferenceScript :: Maybe (Versioned Script)
    } deriving (Show, Eq)

instance FromJSON UtxosAtAddressResponse where
    parseJSON = withObject "UtxosAtAddressResponse" $ \o -> do
        uaarCursor <- o .:? "next_cursor"
        uaarData   <- o .: "data"
        pure UtxosAtAddressResponse{..}

instance FromJSON UtxosAtAddressData where
    parseJSON = withObject "UtxosAtAddressData" $ \o -> do
        uaadTxHash          <- o .: "tx_hash" <&> PV1.TxId
        uaadIndex           <- o .: "index"
        uaadAddress         <- o .: "address" >>= maybe (fail "bech32ToAddress TxDetailsOutput") pure . bech32ToAddress
        uaadValue           <- o .: "assets" <&> mconcat . fmap unMaestro
        uaadDatum           <- o .:? "datum" <&> fmap unMaestro
        uaadReferenceScript <- o .:? "reference_script" <&> fmap unMaestro
        pure UtxosAtAddressData{..}

deriving via BuiltinByteString instance FromJSON (Maestro PV1.TxId)

instance FromJSON (Maestro C.Value) where
    parseJSON = withObject "Bf Value" $ \o -> (,) <$> o .: "unit" <*> o .: "amount" >>= \case
        (J.String "lovelace", amt) -> readAmt amt <&> Maestro . C.lovelaceToValue
        (J.String txt       , amt) -> do
            let (policy, name) = T.splitAt 56 txt
            amt' <- readAmt amt
            assetName <-  maybe (fail "Name from hex") (pure . C.AssetName) $ T.decodeHex name
            policyId <- (either (fail . ("Policy ID from hex:" <> ) . show) (pure . C.PolicyId)
                . C.deserialiseFromRawBytesHex C.AsScriptHash) $ T.encodeUtf8 policy
            let assetId = C.AssetId policyId assetName
            pure $ Maestro $ C.valueFromList [(assetId, amt')]
        _                          -> mzero
        where
            readAmt = \case
                (J.String txt) -> maybe (fail "Read amount from string") (pure . fromInteger) $ readMaybe $ T.unpack txt
                (J.Number num) -> either (const $ fail "Non-integer amount") (pure . fromInteger) $ floatingOrInteger @Double num
                _              -> fail "Parse amount"

instance FromJSON (Maestro (DatumHash, DatumFromQuery)) where
    parseJSON = withObject "Maestro datum" $ \o -> do
        dh  <- o .: "hash"
        dfq <- o .: "type" >>= \case
            J.String "hash"   -> pure DatumUnknown
            J.String "inline" -> o .: "bytes" <&>  DatumInline . Datum
            _                 -> fail "maestro datum type"
        pure $ Maestro (dh, dfq)

instance FromJSON (Maestro (Versioned Script)) where
    parseJSON =  withObject "Maestro versioned script" $ \o -> do
        script <- o .: "bytes" >>= J.withText "script bytes" (maybe (fail "scriptFromCBOR") (pure . Script) . scriptFromCBOR)
        lang   <- o .: "type" >>= \case
            J.String "plutusv1" -> pure PlutusV1
            J.String "plutusv2" -> pure PlutusV2
            _                   -> fail "script language"
        pure $ Maestro $ Versioned script lang

instance ToHttpApiData (Maestro PV1.TxId) where
    toUrlPiece = encodeHex . fromBuiltin @BuiltinByteString . coerce

deriving via (Maestro PV1.TxId) instance ToHttpApiData (Maestro ScriptHash)

instance ToHttpApiData (Maestro AssetClass) where
    toUrlPiece (Maestro (AssetClass (CurrencySymbol cs, TokenName token))) = T.encodeHex (fromBuiltin cs) <> T.decodeUtf8 (fromBuiltin token)
