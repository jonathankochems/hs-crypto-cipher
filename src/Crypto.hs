module Crypto where

import Crypto.Internal (cipherInit, makeKey, Blowfish, ecbEncrypt, ecbDecrypt)

-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.
-- | create creates a Key 
createKey :: String -> Blowfish
createKey = either (error . show) cipherInit . makeKey

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
encrypt :: Blowfish -> String -> String
encrypt ctx = ecbEncrypt ctx

decrypt :: Blowfish -> String -> String 
decrypt ctx = ecbDecrypt ctx 

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
encryptMakeKey :: String -> String -> String
encryptMakeKey key = ecbEncrypt ctx
  where ctx = createKey key

decryptMakeKey :: String -> String -> String 
decryptMakeKey key = ecbDecrypt ctx 
  where ctx = createKey key

