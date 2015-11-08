-- |
-- Module      : Crypto.Cipher.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : Excellent
--
-- symmetric cipher basic types
--
{-# LANGUAGE DeriveDataTypeable #-}
module Crypto.Cipher.Types
    (
    -- * Cipher classes
      Cipher(..)
    , StreamCipher(..)
    , DataUnitOffset
    , KeySizeSpecifier(..)
    , KeyError(..)
    , AEADMode(..)
    -- * Key type and constructor
    , Key
    , makeKey
    -- * Initial Vector type and constructor
    , IV
    -- * Authentification Tag
    , AuthTag(..)
    ) where

import Data.SecureMem
import Data.Byteable
import Crypto.Cipher.Types.Base
import Crypto.Cipher.Types.Stream

-- | Create a Key for a specified cipher
makeKey :: (ToSecureMem b, Cipher c) => b -> Either KeyError (Key c)
makeKey b = toKey undefined
  where sm    = toSecureMem b
        smLen = byteableLength sm
        toKey :: Cipher c => c -> Either KeyError (Key c)
        toKey cipher = case cipherKeySize cipher of
            KeySizeRange mi ma | smLen < mi -> Left KeyErrorTooSmall
                               | smLen > ma -> Left KeyErrorTooBig
                               | otherwise  -> Right $ Key sm
            KeySizeEnum l | smLen `elem` l  -> Right $ Key sm
                          | otherwise       -> Left $ KeyErrorInvalid ("valid size: " ++ show l)
            KeySizeFixed v | smLen == v     -> Right $ Key sm
                           | otherwise      -> Left $ KeyErrorInvalid ("valid size: " ++ show v)
