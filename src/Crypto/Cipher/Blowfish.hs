{-# LANGUAGE CPP #-}
-- |
-- Module      : Crypto.Cipher.Blowfish
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : good
--
module Crypto.Cipher.Blowfish
    ( Blowfish, ecbEncrypt, ecbDecrypt ) where

import Data.ByteString hiding (withByteArray)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS
import Foreign.Ptr (plusPtr, Ptr)
import           Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)

import Crypto.Cipher.Types
import Crypto.Cipher.Types.Base
import Crypto.Cipher.Blowfish.Primitive

-- | variable keyed blowfish state
newtype Blowfish = Blowfish Context

-- | Create a bytestring from a Secure Mem
secureMemToByteString :: ByteString -> ByteString
secureMemToByteString sm = pack $ unpack sm

instance Cipher Blowfish where
    cipherName _    = "blowfish"
    cipherKeySize _ = KeySizeRange 6 56
    cipherInit (Key k) = either error Blowfish $ initBlowfish (secureMemToByteString k)

ecbEncrypt (Blowfish bf) = encrypt bf
ecbDecrypt (Blowfish bf) = decrypt bf
