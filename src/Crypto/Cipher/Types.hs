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
--    , AEADMode(..)
    -- * Key type and constructor
    , Key
    , makeKey
    -- * Initial Vector type and constructor
    , IV
    -- * Authentification Tag
    , AuthTag(..)
    ) where

import Data.SecureMem
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Crypto.Cipher.Types.Base

-- | Create a Key for a specified cipher
makeKey :: (Cipher c) => ByteString -> Either KeyError (Key c)
makeKey b = toKey undefined
  where sm    = toSecureMem b
        smLen = B.length b
        toKey :: Cipher c => c -> Either KeyError (Key c)
        toKey cipher = case cipherKeySize cipher of
            KeySizeRange mi ma | smLen < mi -> Left KeyErrorTooSmall
                               | smLen > ma -> Left KeyErrorTooBig
                               | otherwise  -> Right $ Key sm
            KeySizeEnum l | smLen `elem` l  -> Right $ Key sm
                          | otherwise       -> Left $ KeyErrorInvalid ("valid size: " ++ show l)
            KeySizeFixed v | smLen == v     -> Right $ Key sm
                           | otherwise      -> Left $ KeyErrorInvalid ("valid size: " ++ show v)
