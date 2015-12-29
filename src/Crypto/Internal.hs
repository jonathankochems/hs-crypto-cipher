-- |
-- Module      : Crypto.Cipher.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : Excellent
--
-- symmetric cipher basic types
--
module Crypto.Internal
    (
    -- * Cipher classes
      KeySizeSpecifier(..)
    , KeyError(..)
    , Key(..)
    -- * Key type and constructor
    , Key
    , makeKey
    -- * Blowfish
    , Context(..)
    , myPack
    , myUnpack
    ) where

import Data.Char (ord,chr)
import Crypto.Blowfish (Context(..), initBlowfish)

-- | Create a Key for a specified cipher
makeKey :: String -> Either KeyError (Key Context)
makeKey b' = key
  where b     = myUnpack b'
        smLen = length b
        key :: Either KeyError (Key Context)
        key | smLen < 6  = Left KeyErrorTooSmall
            | smLen > 56 = Left KeyErrorTooBig
            | otherwise  = Right $ Key b

-- | Possible Error that can be reported when initializating a key
data KeyError =
      KeyErrorTooSmall
    | KeyErrorTooBig
    | KeyErrorInvalid String
    deriving (Show,Eq)

-- | Different specifier for key size in bytes
data KeySizeSpecifier =
      KeySizeRange Int Int -- ^ in the range [min,max]
    | KeySizeEnum  [Int]   -- ^ one of the specified values
    | KeySizeFixed Int     -- ^ a specific size
    deriving (Show,Eq)

-- | a Key parametrized by the cipher
newtype Key c = Key [Int] deriving (Eq)

myPack :: [Int] -> String 
myPack   = map chr

myUnpack :: String -> [Int]
myUnpack = map (fromIntegral . ord)

