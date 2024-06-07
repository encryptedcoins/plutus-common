{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}

module PlutusAppsExtra.IO.Tx.Internal where

import           Cardano.Address.Derivation (XPrv)
import           Cardano.Api                (SigningKey (..))
import           Cardano.Api.Tx.Sign        (ShelleyWitnessSigningKey (WitnessPaymentExtendedKey))
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Default               (Default (..))
import           Data.Word                  (Word64)
import           GHC.Generics               (Generic)
import           Ledger                     (CardanoTx (..), addCardanoTxWitness)

data TxState = Rejected | Pending | Failed | Timedout | Onchain | Rolledback
    deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data AwaitTxParameters = MkAwaitTxParameters
    { maxAttempts   :: !Int
    -- ^ Max number of attempts before give up.
    , checkInterval :: !Int
    -- ^ Wait time for each attempt (in microseconds).
    , confirmations :: !Word64
    -- ^ Min number of block confirmation. __NOTE:__ We might wait for more blocks than what is mentioned here but certainly not less.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Default AwaitTxParameters where
    def = MkAwaitTxParameters
          { maxAttempts   = 10
          , checkInterval = 3_000_000
          , confirmations = 1
          }

addCardanoTxSignature :: XPrv -> CardanoTx -> CardanoTx
addCardanoTxSignature privKey = addCardanoTxWitness (WitnessPaymentExtendedKey $  PaymentExtendedSigningKey privKey)
