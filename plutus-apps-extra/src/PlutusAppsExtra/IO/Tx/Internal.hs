{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE DeriveAnyClass #-}

module PlutusAppsExtra.IO.Tx.Internal where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data TxState = Rejected | Pending | Failed | Timedout | Onchain | Rolledback
    deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)