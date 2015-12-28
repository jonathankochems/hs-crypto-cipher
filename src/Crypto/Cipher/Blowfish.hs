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

import Data.Char (ord,chr)

import Crypto.Internal
import Crypto.Cipher.Blowfish.Primitive

-- | variable keyed blowfish state
newtype Blowfish = Blowfish Context

instance Cipher Blowfish where
    cipherName _    = "blowfish"
    cipherKeySize _ = KeySizeRange 6 56
    cipherInit (Key k) = either error Blowfish $ initBlowfish k

myPack   = map chr
myUnpack = map ord 

ecbEncrypt, ecbDecrypt :: Blowfish -> String -> String 
ecbEncrypt (Blowfish bf) = myPack . encrypt bf . myUnpack 
ecbDecrypt (Blowfish bf) = myPack . decrypt bf . myUnpack
