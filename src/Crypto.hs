module Crypto where

import Crypto.Cipher.Types (cipherInit, makeKey)
import Crypto.Cipher.Blowfish (Blowfish, ecbEncrypt, ecbDecrypt)

-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.

initC :: String -> Blowfish
initC = either (error . show) cipherInit . makeKey

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
cryptKey :: String -> String -> String
cryptKey key = ecbEncrypt ctx
  where ctx = initC key

decryptKey :: String -> String -> String 
decryptKey key = ecbDecrypt ctx 
  where ctx = initC key

