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
    -- * Key type and constructor
      KeyError(..)
    , Key(..)
    , makeKey
    -- * pack/unpack
    , myPack
    , myUnpack
    ) where

import Data.Char (ord,chr)

-- | Create a Key for a specified cipher
makeKey :: String -> Key
makeKey b' = either (error . show) id key
  where b     = myUnpack b'
        smLen = length b
        key :: Either KeyError Key
        key | smLen < 6  = Left KeyErrorTooSmall
            | smLen > 56 = Left KeyErrorTooBig
            | otherwise  = Right $ Key b

-- | Possible Error that can be reported when initializating a key
data KeyError =
      KeyErrorTooSmall
    | KeyErrorTooBig
    deriving (Show,Eq)

-- | a Key parametrized by the cipher
newtype Key = Key [Int] deriving (Eq)

myPack :: [Int] -> String 
myPack   = map chr

myUnpack :: String -> [Int]
myUnpack = map (fromIntegral . ord)

