-- |
-- Module      : Crypto.Cipher.Types.Base
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : Excellent
--
-- symmetric cipher basic types
--
module Crypto.Cipher.Types.Base
    ( KeyError(..)
    , KeySizeSpecifier(..)
    , Key(..)
    , IV(..)
    , Cipher(..)
    , AuthTag(..)
    , DataUnitOffset
    ) where


import Data.Word
import Data.ByteString (ByteString)

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

-- | Offset inside an XTS data unit, measured in block size.
type DataUnitOffset = Word32

-- | a Key parametrized by the cipher
newtype Key c = Key ByteString deriving (Eq)

-- | an IV parametrized by the cipher
newtype IV c = IV ByteString deriving (Eq)

-- | Authentification Tag for AE cipher mode
newtype AuthTag = AuthTag ByteString
    deriving (Show,Eq)

-- | Symmetric cipher class.
class Cipher cipher where
    -- | Initialize a cipher context from a key
    cipherInit    :: Key cipher -> cipher
    -- | Cipher name
    cipherName    :: cipher -> String
    -- | return the size of the key required for this cipher.
    -- Some cipher accept any size for key
    cipherKeySize :: cipher -> KeySizeSpecifier
