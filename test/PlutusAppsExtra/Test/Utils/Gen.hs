module PlutusAppsExtra.Test.Utils.Gen where

import           Cardano.Node.Emulator  ()
import           Control.Monad          (replicateM)
import qualified Data.ByteString        as BS
import           Ledger                 (Address (..), PaymentPubKey (..), PubKey (..), PubKeyHash (..), TxId (..), TxOutRef (..),
                                         pubKeyAddress)
import qualified Plutus.V1.Ledger.Bytes as LedgerBytes
import           Plutus.V2.Ledger.Api   (Credential (..), StakingCredential (StakingHash), toBuiltin)
import           System.Random          (randomIO, randomRIO)
import           Test.QuickCheck        (Arbitrary (..), choose, generate)

genPubKeyAddress :: IO Address
genPubKeyAddress = do
    lb <- LedgerBytes.fromBytes <$> generate arbitrary
    pure $ pubKeyAddress (PaymentPubKey (PubKey lb)) Nothing

genPubKeyAddressWithStakingHash :: IO Address
genPubKeyAddressWithStakingHash = do
    Address cred _ <- genPubKeyAddress
    pkh <- PubKeyHash . toBuiltin . BS.pack <$> replicateM 28 (generate arbitrary)
    pure $ Address cred $ Just (StakingHash (PubKeyCredential pkh))

-- An existing arbitrary instance is rejected by a local node
genTxOutRef :: IO TxOutRef
genTxOutRef = TxOutRef <$> (TxId . toBuiltin . BS.pack <$> replicateM 32 (generate arbitrary)) <*> randomRIO (1,40)

genCollateral :: IO (Maybe TxOutRef)
genCollateral = randomIO >>= \b -> if b then pure Nothing else Just <$> genTxOutRef
