{-# LANGUAGE CPP #-}
-- |
-- Module      : Crypto.Cipher.Blowfish
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : good
--
module Crypto.Cipher.Blowfish
    ( Blowfish, ecbEncrypt, ecbDecrypt
    --, Blowfish64
    --, Blowfish128
    --, Blowfish256
    --, Blowfish448
    ) where

import Data.ByteString hiding (withByteArray)
import           Data.ByteArray  (ScrubbedBytes, withByteArray)
import qualified Data.ByteArray  as BA
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

---- | 64 bit keyed blowfish state
--newtype Blowfish64 = Blowfish64 Context

---- | 128 bit keyed blowfish state
--newtype Blowfish128 = Blowfish128 Context

---- | 256 bit keyed blowfish state
--newtype Blowfish256 = Blowfish256 Context

---- | 448 bit keyed blowfish state
--newtype Blowfish448 = Blowfish448 Context

-- | Create a bytestring from a Secure Mem
secureMemToByteString :: ScrubbedBytes -> ByteString
secureMemToByteString sm =
    BS.unsafeCreate sz $ \dst ->
    BA.withByteArray sm $ \src ->
    BS.memcpy dst src (fromIntegral sz)
  where sz = BA.length sm
        --withByteArray :: ScrubbedBytes -> (Ptr Word8 -> IO b) -> IO b
        --withByteArray b f = withForeignPtr fptr $ \ptr -> f (ptr `plusPtr` off)
        --   where (fptr, off, _) = BS.toForeignPtr b

instance Cipher Blowfish where
    cipherName _    = "blowfish"
    cipherKeySize _ = KeySizeRange 6 56
    cipherInit (Key k) = either error Blowfish $ initBlowfish (secureMemToByteString k)

--instance BlockCipher Blowfish where
--    blockSize _ = 8
ecbEncrypt (Blowfish bf) = encrypt bf
ecbDecrypt (Blowfish bf) = decrypt bf

-- #define INSTANCE_CIPHER(CSTR, NAME, KEYSIZE) \
--instance Cipher CSTR where \
--    { cipherName _ = NAME \
--    ; cipherKeySize _ = KeySizeFixed KEYSIZE \
--    ; cipherInit k = either error CSTR $ initBlowfish (toBytes k) \
--    }; \
--instance BlockCipher CSTR where \
--    { blockSize _ = 8 \
--    ; ecbEncrypt (CSTR bf) = encrypt bf \
--    ; ecbDecrypt (CSTR bf) = decrypt bf \
--    };

--INSTANCE_CIPHER(Blowfish64, "blowfish64", 8)
--INSTANCE_CIPHER(Blowfish128, "blowfish128", 16)
--INSTANCE_CIPHER(Blowfish256, "blowfish256", 32)
--INSTANCE_CIPHER(Blowfish448, "blowfish448", 56)
