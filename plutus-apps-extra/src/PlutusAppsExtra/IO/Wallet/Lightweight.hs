module PlutusAppsExtra.IO.Wallet.Lightweight where

import           Cardano.Mnemonic                   (SomeMnemonic)
import           Control.Monad.Catch                (MonadThrow (throwM))
import           Ledger                             (Address, NetworkId)
import           PlutusAppsExtra.Api.Blockfrost     (runBfMonad, BlockfrostToken)
import           PlutusAppsExtra.IO.Blockfrost      (getAccountAssociatedAddresses)
import           PlutusAppsExtra.IO.Wallet.Internal (walletKeysFromMnemonic, walletKeysToStakeAddressBech32)
import           PlutusAppsExtra.Types.Error        (WalletError (..))
import           PlutusAppsExtra.Utils.Address      (bech32ToStakePubKeyHash)

deriveLightweightAddresses :: NetworkId -> BlockfrostToken -> SomeMnemonic -> IO (Maybe [Address])
deriveLightweightAddresses networkId token mnemonic = do
    keys <- either throwM pure $ walletKeysFromMnemonic mnemonic
    stakeBech32 <- either throwM pure $ walletKeysToStakeAddressBech32 keys networkId
    spkh <- maybe (throwM $ UnparsableAddress stakeBech32) pure $ bech32ToStakePubKeyHash stakeBech32
    runBfMonad networkId token $ getAccountAssociatedAddresses spkh