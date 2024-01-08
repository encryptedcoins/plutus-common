{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

module PlutusAppsExtra.Utils.Maestro where

import           Control.Monad                 (forM)
import           Data.Aeson                    (FromJSON (..), withObject, (.:), (.:?))
import qualified Data.Aeson                    as J
import           Data.Coerce                   (coerce)
import           Data.Functor                  ((<&>))
import           Data.Maybe                    (catMaybes)
import qualified Data.Time                     as Time
import           GHC.Records                   (HasField (..))
import           Ledger                        (Address, Datum (..), DatumFromQuery (..), DatumHash, PubKeyHash (..), Slot (..),
                                                StakePubKeyHash (..), TxId (..))
import           Plutus.V1.Ledger.Api          (BuiltinByteString, CurrencySymbol (..), TokenName (..), fromBuiltin, toBuiltin)
import           Plutus.V1.Ledger.Value        (AssetClass (..))
import           PlutusAppsExtra.Utils.Address (bech32ToAddress, bech32ToStakePubKeyHash)
import           Servant.API                   (ToHttpApiData (..))
import           Text.Hex                      (decodeHex, encodeHex)
import qualified Text.Hex                      as T
import           Text.Read                     (readMaybe)

newtype Maestro a = Maestro {getMaestro :: a}

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
    { ambrData      :: [AssetMintsAndBurnsData]
    , ambrCursor    :: Maybe Cursor
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
        tdoDatum   <- o .:? "datum" <&> fmap getMaestro
        pure TxDetailsOutput{..}

instance FromJSON (Maestro (DatumHash, DatumFromQuery)) where
    parseJSON = withObject "Maestro datum" $ \o -> do
        dh  <- o .: "hash"
        dfq <- o .: "type" >>= \case
            J.String "hash"   -> pure DatumUnknown
            J.String "inline" -> o .: "bytes" <&>  DatumInline . Datum
            _ -> fail "maestro datum type"
        pure $ Maestro (dh, dfq)

instance ToHttpApiData (Maestro TxId) where
    toUrlPiece = encodeHex . fromBuiltin @BuiltinByteString . coerce

instance ToHttpApiData (Maestro AssetClass) where
    toUrlPiece (Maestro (AssetClass (CurrencySymbol cs, TokenName token))) = T.encodeHex $ fromBuiltin $ cs <> token