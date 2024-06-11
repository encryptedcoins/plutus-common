module PlutusAppsExtra.Test.Utils.Gen where

import           Cardano.Node.Emulator ()
import           Control.Monad         (replicateM)
import qualified Data.ByteString       as BS
import           Ledger                (Address (..), PaymentPubKey (..), PubKey (..), PubKeyHash (..), TxOutRef (..), pubKeyAddress)
import qualified PlutusLedgerApi.V1.Tx as PV1
import           PlutusLedgerApi.V3    (Credential (..), StakingCredential (..), fromBytes, toBuiltin)
import           Test.QuickCheck       (Arbitrary (..), choose)
import           Test.QuickCheck.Gen   (Gen)

genPubKeyAddress :: Gen Address
genPubKeyAddress = do
    lb <- fromBytes <$> arbitrary
    pure $ pubKeyAddress (PaymentPubKey (PubKey lb)) Nothing

genPubKeyAddressWithStakingHash :: Gen Address
genPubKeyAddressWithStakingHash = do
    Address cred _ <- genPubKeyAddress
    pkh <- PubKeyHash . toBuiltin . BS.pack <$> replicateM 28 arbitrary
    pure $ Address cred $ Just (StakingHash (PubKeyCredential pkh))

-- An existing arbitrary instance is rejected by a local node
genTxOutRef :: Gen TxOutRef
genTxOutRef = TxOutRef
    <$> (PV1.TxId . toBuiltin . BS.pack <$> replicateM 32 arbitrary)
    <*> choose (1,40)

genCollateral :: Gen (Maybe TxOutRef)
genCollateral = arbitrary >>= \b -> if b then pure Nothing else Just <$> genTxOutRef
