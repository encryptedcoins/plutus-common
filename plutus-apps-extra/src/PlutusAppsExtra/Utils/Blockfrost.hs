{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

module PlutusAppsExtra.Utils.Blockfrost where

import           Cardano.Api                   (SerialiseAddress (serialiseAddress), StakeAddress)
import qualified Cardano.Api                   as C
import           Cardano.Api.Shelley           (PoolId)
import           Codec.Serialise               (deserialise)
import           Control.Monad                 (mzero)
import           Data.Aeson                    (FromJSON (..), ToJSON, withObject, withText, (.:))
import qualified Data.Aeson                    as J
import qualified Data.ByteString.Lazy          as LBS
import           Data.Coerce                   (coerce)
import           Data.Functor                  ((<&>))
import qualified Data.Text                     as T
import           Deriving.Aeson                (CamelToSnake, CustomJSON (CustomJSON), FieldLabelModifier, Generic, StripPrefix)
import           Ledger                        (Address, Datum (..), DatumHash (..), ScriptHash, TxId (..), ValidatorHash)
import           Plutus.Script.Utils.Value     (AssetClass (..), TokenName (..))
import           Plutus.V1.Ledger.Api          (BuiltinByteString, CurrencySymbol (..), fromBuiltin, toBuiltin)
import qualified Plutus.V2.Ledger.Api          as P
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import           Servant.API                   (ToHttpApiData (..))
import           Text.Hex                      (decodeHex, encodeHex)
import qualified Text.Hex                      as T
import           Text.Read                     (readMaybe)

data AccDelegationHistoryResponse = AccDelegationHistoryResponse
    { adhrActiveEpoch :: Int
    , adhrTxHash      :: TxId
    , adhrAmount      :: Int
    , adhrPoolId      :: PoolId
    } deriving Show

instance FromJSON AccDelegationHistoryResponse where
    parseJSON = withObject "Tx delegation certificate response" $ \o -> do
        adhrActiveEpoch <- o .: "active_epoch"
        adhrTxHash      <- o .: "tx_hash" <&> TxId
        adhrAmount      <- o .: "amount" >>= maybe (fail "amount") pure . readMaybe
        adhrPoolId      <- o .: "pool_id"
        pure AccDelegationHistoryResponse{..}

data TxDelegationsCertsResponse = TxDelegationsCertsResponse
    { tdcrIndex       :: Int
    , tdcrCertIndex   :: Int
    , tdcrAddress     :: Address
    , tdcrPoolId      :: PoolId
    , tdcrActiveEpoch :: Int
    } deriving Show

instance FromJSON TxDelegationsCertsResponse where
    parseJSON = withObject "Tx delegation certificate response" $ \o -> do
        tdcrIndex       <- o .: "index"
        tdcrCertIndex   <- o .: "cert_index"
        tdcrAddress     <- o .: "address" >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
        tdcrPoolId      <- o .: "pool_id"
        tdcrActiveEpoch <- o .: "active_epoch"
        pure TxDelegationsCertsResponse{..}

data TxUtxoResponse = TxUtxoResponse
    { turTxHash  :: TxId
    , turInputs  :: [TxUtxoResponseInput]
    , turOutputs :: [TxUtxoResponseOutput]
    } deriving (Show, Eq)

instance FromJSON TxUtxoResponse where
    parseJSON = withObject "Tx UTXOs response" $ \o -> do
        turTxHash  <- o .: "hash"
        turInputs  <- o .: "inputs"
        turOutputs <- o .: "outputs"
        pure TxUtxoResponse{..}

data TxUtxoResponseInput = TxUtxoResponseInput
    { turiAddress :: Address
    } deriving (Show, Eq)

instance FromJSON TxUtxoResponseInput where
    parseJSON = withObject "Tx UTXOs response input" $ \o -> do
        turiAddress <- o .: "address" >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
        pure TxUtxoResponseInput{..}

data TxUtxoResponseOutput = TxUtxoResponseOutput
    { turoAddress             :: Address
    , turoAmount              :: C.Value
    , turoIdx                 :: Integer
    , turoDatumHash           :: Maybe P.DatumHash
    , turoInlineDatum         :: Maybe Datum
    , turoReferenceScriptHash :: Maybe P.ScriptHash
    } deriving (Show, Eq)

instance FromJSON TxUtxoResponseOutput where
    parseJSON = withObject "Tx UTXOs response output" $ \o -> do
        turoAddress   <- o .: "address" >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
        turoAmount    <- o .: "amount" <&> mconcat . fmap unBf
        turoIdx       <- o .: "output_index"
        turoDatumHash <- o .: "data_hash"  >>= \case
            J.Null -> pure Nothing
            J.String dHash -> Just . DatumHash <$> toBbs dHash
            val -> fail $ show val
        turoInlineDatum <- o .: "inline_datum"  >>= \case
            J.Null -> pure Nothing
            J.String dat -> maybe (fail "not a hex") (pure . Just . Datum . deserialise . LBS.fromStrict) $ decodeHex dat
            val -> fail $ show val
        turoReferenceScriptHash <- o .: "reference_script_hash" >>= \case
            J.Null -> pure Nothing
            J.String sHash ->Just . P.ScriptHash <$> toBbs sHash
            val -> fail $ show val
        pure TxUtxoResponseOutput{..}
        where toBbs = maybe (fail "not a hex") (pure . toBuiltin) . decodeHex

data BfMintingPolarity = BfMint | BfBurn
    deriving (Show, Eq, Enum)

instance FromJSON BfMintingPolarity where
    parseJSON = withText "Bf minting polarity" $ \case
        "minted" -> pure BfMint
        "burned" -> pure BfBurn
        _        -> mzero

data AssetAddressesResponse = AssetAddressesResponse
    { adrAddress  :: Address
    , adrQuantity :: Integer
    } deriving (Show, Generic)
      deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "adr", CamelToSnake]] AssetAddressesResponse

data AssetTxsResponse = AssetTxsResponse
    { atrTxHash  :: TxId
    , atrTxIndex :: Integer
    } deriving (Show, Generic)

instance FromJSON AssetTxsResponse where
    parseJSON = withObject "Asset txs response" $ \o -> do
        atrTxHash  <- o .: "tx_hash" <&> TxId
        atrTxIndex <- o .: "tx_index"
        pure AssetTxsResponse{..}

data AssetHistoryResponse = AssetHistoryResponse
    { ahrTxHash          :: TxId
    , ahrMintingPolarity :: BfMintingPolarity
    , ahrAmount          :: Integer
    } deriving Show

instance FromJSON AssetHistoryResponse where
    parseJSON = withObject "Asset history response" $ \o -> do
        ahrTxHash          <- o .: "tx_hash"
        ahrMintingPolarity <- o .: "action"
        ahrAmount          <- o .: "amount" >>= maybe mzero pure . readMaybe . T.unpack
        pure AssetHistoryResponse{..}

newtype Bf a = Bf {unBf :: a}
    deriving Functor

deriving newtype instance Show a => Show (Bf a)

data BfOrder = Asc | Desc

instance FromJSON (Bf C.Value) where
    parseJSON = withObject "Bf Value" $ \o -> (,) <$> o .: "unit" <*> o .: "quantity" >>= \case
        (J.String "lovelace", amt) -> readAmt amt <&> Bf . C.lovelaceToValue
        (J.String txt       , amt) -> do
            let (policy, name) = T.splitAt 56 txt
            amt' <- readAmt amt
            assetName <-  maybe (fail "Name from hex") (pure . C.AssetName) $ T.decodeHex name
            policyId <- maybe (fail "Policy ID from hex")
                (either (fail . show) (pure . C.PolicyId) . C.deserialiseFromRawBytesHex C.AsScriptHash)
                $ T.decodeHex policy
            let assetId = C.AssetId policyId assetName
            pure $ Bf $ C.valueFromList [(assetId, amt')]
        _                          -> mzero
        where
            readAmt =  maybe (fail "Read amount from string") (pure . fromInteger) . readMaybe . T.unpack

instance ToHttpApiData BfOrder where
    toUrlPiece = \case
        Asc  -> "asc"
        Desc -> "desc"

instance ToHttpApiData (Bf StakeAddress) where
    toUrlPiece (Bf addr) = serialiseAddress addr

instance ToHttpApiData (Bf TxId) where
    toUrlPiece = T.dropAround (== '\"') . T.pack . show

instance ToHttpApiData (Bf AssetClass) where
    toUrlPiece (Bf (AssetClass (CurrencySymbol cs, TokenName token))) = T.encodeHex $ fromBuiltin $ cs <> token

instance ToHttpApiData (Bf DatumHash) where
    toUrlPiece = encodeHex . fromBuiltin @BuiltinByteString . coerce

deriving via (Bf DatumHash) instance ToHttpApiData (Bf ScriptHash)
deriving via (Bf DatumHash) instance ToHttpApiData (Bf ValidatorHash)