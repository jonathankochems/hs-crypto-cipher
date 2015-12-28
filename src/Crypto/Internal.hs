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
      Cipher(..)
    , KeySizeSpecifier(..)
    , KeyError(..)
    , Key(..)
    -- * Key type and constructor
    , Key
    , makeKey
    -- * Blowfish
    , Blowfish
    , ecbEncrypt
    , ecbDecrypt
    ) where

import Data.Char (ord,chr)
import Crypto.Cipher.Blowfish.Primitive (Context, initBlowfish,encrypt,decrypt)


-- | Create a Key for a specified cipher
makeKey :: (Cipher c) => String -> Either KeyError (Key c)
makeKey b' = toKey undefined
  where b     = myUnpack b'
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

-- | Symmetric cipher class.
class Cipher cipher where
    -- | Initialize a cipher context from a key
    cipherInit    :: Key cipher -> cipher
    -- | Cipher name
    cipherName    :: cipher -> String
    -- | return the size of the key required for this cipher.
    -- Some cipher accept any size for key
    cipherKeySize :: cipher -> KeySizeSpecifier


newtype Blowfish = Blowfish Context

instance Cipher Blowfish where
    cipherName _    = "blowfish"
    cipherKeySize _ = KeySizeRange 6 56
    cipherInit (Key k) = either error Blowfish $ initBlowfish k

myPack :: [Int] -> String 
myPack   = map chr

myUnpack :: String -> [Int]
myUnpack = map (fromIntegral . ord)

ecbEncrypt, ecbDecrypt :: Blowfish -> String -> String 
ecbEncrypt (Blowfish bf) = myPack . encrypt bf . myUnpack 
ecbDecrypt (Blowfish bf) = myPack . decrypt bf . myUnpack

