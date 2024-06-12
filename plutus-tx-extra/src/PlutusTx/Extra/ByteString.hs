{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module PlutusTx.Extra.ByteString where

import           PlutusTx.Builtins                 hiding (head)
import           PlutusTx.Prelude                  hiding ((<>))
import           Prelude                           (Char, String)

----------------- Conversions to BuiltinByteString type --------------------

class ToBuiltinByteString a where
    toBytes :: a -> BuiltinByteString

instance ToBuiltinByteString BuiltinByteString where
    {-# INLINABLE toBytes #-}
    toBytes = id

instance ToBuiltinByteString Bool where
    {-# INLINABLE toBytes #-}
    toBytes False = consByteString 0 emptyByteString
    toBytes True  = consByteString 1 emptyByteString

instance ToBuiltinByteString Integer where
    {-# INLINABLE toBytes #-}
    toBytes n = consByteString r $ if q > 0 then toBytes q else emptyByteString
        where (q, r) = divMod n 256

instance (ToBuiltinByteString a, ToBuiltinByteString b) => ToBuiltinByteString (a, b) where
    {-# INLINABLE toBytes #-}
    toBytes (x, y) = toBytes x `appendByteString` toBytes y

instance (ToBuiltinByteString a, ToBuiltinByteString b, ToBuiltinByteString c) => ToBuiltinByteString (a, b, c) where
    {-# INLINABLE toBytes #-}
    toBytes (x, y, z) = toBytes x `appendByteString` toBytes y `appendByteString` toBytes z

instance (ToBuiltinByteString a, ToBuiltinByteString b, ToBuiltinByteString c, ToBuiltinByteString d) => ToBuiltinByteString (a, b, c, d) where
    {-# INLINABLE toBytes #-}
    toBytes (x, y, z, w) = toBytes x `appendByteString` toBytes y `appendByteString` toBytes z `appendByteString` toBytes w

instance ToBuiltinByteString a => ToBuiltinByteString [a] where
    {-# INLINABLE toBytes #-}
    toBytes = foldr (appendByteString . toBytes) emptyByteString

---------------- Conversions from BuiltinByteString type -------------------

{-# INLINABLE byteStringToList #-}
byteStringToList :: BuiltinByteString -> [Integer]
byteStringToList bs
    | equalsByteString bs emptyByteString = []
    | otherwise                           = indexByteString bs 0 : byteStringToList (dropByteString 1 bs)

{-# INLINABLE byteStringToInteger #-}
byteStringToInteger :: BuiltinByteString -> Integer
byteStringToInteger bs = foldr (\d n -> 256*n + d) 0 (byteStringToList bs)

-------------------- Conversions with strings and chars --------------------

stringToBytes :: String -> BuiltinByteString
stringToBytes str = foldr (consByteString . g) emptyByteString (f str)
    where
        f s = if length s > 1 then take 2 s : f (drop 2 s) else []
        g s = charToHex (head s) * 16 + charToHex (s !! 1)

charToHex :: Char -> Integer
charToHex '0' = 0
charToHex '1' = 1
charToHex '2' = 2
charToHex '3' = 3
charToHex '4' = 4
charToHex '5' = 5
charToHex '6' = 6
charToHex '7' = 7
charToHex '8' = 8
charToHex '9' = 9
charToHex 'a' = 10
charToHex 'b' = 11
charToHex 'c' = 12
charToHex 'd' = 13
charToHex 'e' = 14
charToHex 'f' = 15
charToHex _   = error ()