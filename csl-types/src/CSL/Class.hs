{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}

module CSL.Class where

import           Cardano.Api.Shelley              (NetworkId)
import           Control.Monad                    (join)
import           Data.Either.Extra                (eitherToMaybe)
import qualified Data.Map                         as Map
import           Data.Maybe                       (Maybe)
import           Data.Text                        (pack, unpack)
import           Ledger                           (DecoratedTxOut (..), _decoratedTxOutAddress, fromCardanoValue,
                                                   stakingCredential, toCardanoValue, toPubKeyHash)
import           Plutus.Script.Utils.Ada          (Ada (getLovelace), fromValue, lovelaceValueOf)
import           PlutusLedgerApi.V2               (CurrencySymbol (..), TokenName (..), TxOutRef (..), Value (..))
import qualified PlutusLedgerApi.V2               as P
import           PlutusAppsExtra.Utils.Address    (addressToBech32, bech32ToAddress)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import           PlutusAppsExtra.Utils.Kupo       (KupoResponse (..))
import           PlutusTx.AssocMap                hiding (mapMaybe)
import           PlutusTx.Prelude                 hiding (Maybe, toList)
import           Prelude                          (repeat, show)
import           Text.Hex                         (decodeHex, encodeHex)
import           Text.Read                        (readMaybe)

import qualified CSL

class FromCSL a b | b -> a where
    fromCSL :: a -> Maybe b

instance FromCSL CSL.TransactionInput TxOutRef where
    fromCSL (CSL.TransactionInput txId ind) = do
        bs <- decodeHex txId
        return $ TxOutRef (P.TxId $ toBuiltin bs) ind

instance FromCSL CSL.TransactionInputs [TxOutRef] where
    fromCSL = Just . mapMaybe fromCSL

instance FromCSL CSL.Value P.Value where
    fromCSL (CSL.Value lovelace mma) = do
        valADA <- fmap lovelaceValueOf $ readMaybe $ unpack lovelace
        let valMA = case mma of
                Nothing -> zero
                Just (CSL.MultiAsset ma) -> Value (g' ma)
        return $ valADA + valMA
        where
            f  (aTxt, bTxt) = do
                a <- TokenName . toBuiltin <$> decodeHex aTxt
                b <- readMaybe $ unpack bTxt :: Maybe Integer
                return (a, b)
            g  = unsafeFromList . mapMaybe f . Map.toList
            f' (kTxt, m) = do
                k <- CurrencySymbol . toBuiltin <$> decodeHex kTxt
                return (k, g m)
            g' = unsafeFromList . mapMaybe f' . Map.toList

instance FromCSL CSL.TransactionOutput DecoratedTxOut where
    fromCSL (CSL.TransactionOutput addrCSL valCSL _ _) = do
        addr <- bech32ToAddress addrCSL
        val  <- join $ eitherToMaybe . toCardanoValue <$> fromCSL valCSL
        let scr = stakingCredential addr
        case toPubKeyHash addr of
          Nothing  -> Nothing
          Just pkh -> return $ PublicKeyDecoratedTxOut pkh scr val Nothing Nothing

instance FromCSL CSL.TransactionOutputs [DecoratedTxOut] where
    fromCSL = Just . mapMaybe fromCSL

instance FromCSL CSL.TransactionUnspentOutput (TxOutRef, DecoratedTxOut) where
    fromCSL (CSL.TransactionUnspentOutput input output) = do
        ref   <- fromCSL input
        txOut <- fromCSL output
        return (ref, txOut)

instance FromCSL CSL.TransactionUnspentOutputs [(TxOutRef, DecoratedTxOut)] where
    fromCSL = Just . mapMaybe fromCSL

instance FromCSL CSL.TransactionUnspentOutputs MapUTXO where
    fromCSL = fmap Map.fromList . fromCSL

class ToCSL a b | a -> b where
    toCSL :: a -> Maybe b

instance ToCSL TxOutRef CSL.TransactionInput where
    toCSL (TxOutRef (P.TxId bbs) ind) =
        Just $ CSL.TransactionInput (encodeHex $ fromBuiltin bbs) ind

instance ToCSL [TxOutRef] CSL.TransactionInputs where
    toCSL = Just . mapMaybe toCSL

instance ToCSL P.Value CSL.Value where
    toCSL val = Just $ CSL.Value (pack $ show $ getLovelace lovelace) (CSL.MultiAsset <$> mma)
        where
            lovelace = fromValue val
            assets   = delete P.adaSymbol $ getValue val
            mma = if assets == empty then Nothing else Just $ g' assets
            g' = Map.fromList . map f' . toList
            f' (k, m) = let kTxt = encodeHex $ fromBuiltin $ unCurrencySymbol k
                        in (kTxt, g m)
            g = Map.fromList . map f . toList
            f (a, b) = let aTxt = encodeHex $ fromBuiltin $ unTokenName a
                           bTxt = pack $ show b
                       in (aTxt, bTxt)

instance ToCSL (DecoratedTxOut, NetworkId) CSL.TransactionOutput where
    toCSL (txOut, networkId) = do
        addrCSL <- addressToBech32 networkId $ _decoratedTxOutAddress txOut
        valCSL  <- toCSL $ fromCardanoValue $ _decoratedTxOutValue txOut
        return $ CSL.TransactionOutput addrCSL valCSL Nothing Nothing

instance ToCSL ([DecoratedTxOut], NetworkId) CSL.TransactionOutputs where
    toCSL (txOuts, network) = Just . mapMaybe toCSL $ zip txOuts $ repeat network

instance ToCSL (TxOutRef, DecoratedTxOut, NetworkId) CSL.TransactionUnspentOutput where
    toCSL (ref, txOut, network) = CSL.TransactionUnspentOutput <$> toCSL ref <*> toCSL (txOut, network)

instance ToCSL ([(TxOutRef, DecoratedTxOut)], NetworkId) CSL.TransactionUnspentOutputs where
    toCSL (xs, network) = Just $ mapMaybe (\(ref, txOut) -> toCSL (ref, txOut, network)) xs

instance ToCSL (MapUTXO, NetworkId) CSL.TransactionUnspentOutputs where
    toCSL (mapUtxo, network) = toCSL (Map.toList mapUtxo, network)

instance ToCSL (KupoResponse, NetworkId) CSL.TransactionUnspentOutput where
    toCSL (KupoResponse{..}, networkId) = do
        let inputCSL = CSL.TransactionInput (encodeHex $ fromBuiltin $ P.getTxId krTxId) krOutputIndex
        addrCSL <- addressToBech32 networkId krAddress
        valCSL  <- toCSL (fromCardanoValue krValue)
        pure $ CSL.TransactionUnspentOutput inputCSL (CSL.TransactionOutput addrCSL valCSL Nothing Nothing)

instance ToCSL ([KupoResponse], NetworkId) CSL.TransactionUnspentOutputs where
    toCSL (responses, networkId) = Just $ mapMaybe (toCSL . (, networkId)) responses