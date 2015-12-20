-- |
-- Module      : Crypto.Cipher.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : Excellent
--
-- symmetric cipher basic types
--
module Crypto.Cipher.Types
    (
    -- * Cipher classes
      Cipher(..)
    , DataUnitOffset
    , KeySizeSpecifier(..)
    , KeyError(..)
    -- * Key type and constructor
    , Key
    , makeKey
    ) where

import Crypto.Cipher.Types.Base
import Data.Char (ord)

myPack :: String -> [Int]
myPack = map (fromIntegral . ord)

-- | Create a Key for a specified cipher
makeKey :: (Cipher c) => String -> Either KeyError (Key c)
makeKey b' = toKey undefined
  where b     = myPack b'
        sm    = {-BA.pack $ B.unpack-} b
        smLen = length b
        toKey :: Cipher c => c -> Either KeyError (Key c)
        toKey cipher = case cipherKeySize cipher of
            KeySizeRange mi ma | smLen < mi -> Left KeyErrorTooSmall
                               | smLen > ma -> Left KeyErrorTooBig
                               | otherwise  -> Right $ Key sm
            KeySizeEnum l | smLen `elem` l  -> Right $ Key sm
                          | otherwise       -> Left $ KeyErrorInvalid ("valid size: " ++ show l)
            KeySizeFixed v | smLen == v     -> Right $ Key sm
                           | otherwise      -> Left $ KeyErrorInvalid ("valid size: " ++ show v)
