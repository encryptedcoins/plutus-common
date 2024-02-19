module PlutusAppsExtra.IO.Wallet.Blockfrost where

import           Cardano.Wallet.Primitive.Types.Address (AddressState (Unused, Used))
import           Control.Monad                          (filterM)
import           Control.Monad.Catch                    (MonadThrow (..))
import           Ledger                                 (Address, StakePubKeyHash (StakePubKeyHash))
import           PlutusAppsExtra.Api.Blockfrost         (MonadBlockfrost)
import           PlutusAppsExtra.IO.Blockfrost          (getAccountAssociatedAddresses, isUsedAddress)
import           PlutusAppsExtra.IO.Wallet.Internal     (HasWallet, InternalWalletError (NoAssociatedAddresses), getWalletPubKeyHash)

ownAddressesFromStakeAddress :: (HasWallet m, MonadBlockfrost m) => Maybe AddressState -> m [Address]
ownAddressesFromStakeAddress addrState = do
    pkh <- getWalletPubKeyHash
    addrs <- maybe (throwM NoAssociatedAddresses) pure =<< getAccountAssociatedAddresses (StakePubKeyHash pkh)
    case addrState of
        Nothing     -> pure addrs
        Just Used   -> filterM isUsedAddress addrs
        Just Unused-> filterM (fmap not . isUsedAddress) addrs