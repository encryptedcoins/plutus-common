{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module CSL where

import           Data.Aeson          (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Map            as Map
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Prelude

data TransactionInput = TransactionInput {
        transaction_id :: Text,
        index          :: Integer
    }
    deriving stock (Eq, Ord, Show, Read, Generic)
    deriving (ToJSON, FromJSON)

type TransactionInputs = [TransactionInput]

newtype MultiAsset = MultiAsset (Map.Map Text (Map.Map Text Text))
    deriving stock (Eq, Ord, Show, Read, Generic)
    deriving (ToJSON, FromJSON)

data Value = Value
    {
        coin        :: Text,
        multiasset  :: Maybe MultiAsset
    }
    deriving stock (Eq, Ord, Show, Read, Generic)
    deriving (ToJSON, FromJSON)

newtype Data = Data Text
    deriving stock (Eq, Ord, Show, Read, Generic)
instance ToJSON Data where
    toJSON (Data text) = object [ "Data" .= text ]
instance FromJSON Data where
    parseJSON = withObject "Data" $ \o -> Data <$> o .: "Data"

data TransactionOutput = TransactionOutput
    {
        address     :: Text,
        amount      :: Value,
        plutus_data :: Maybe Data,
        script_ref  :: Maybe Text
    }
    deriving stock (Eq, Ord, Show, Read, Generic)
    deriving (ToJSON, FromJSON)

type TransactionOutputs = [TransactionOutput]

data TransactionUnspentOutput = TransactionUnspentOutput
    {
        input :: TransactionInput,
        output :: TransactionOutput
    }
    deriving stock (Eq, Ord, Show, Read, Generic)
    deriving (ToJSON, FromJSON)

type TransactionUnspentOutputs = [TransactionUnspentOutput]