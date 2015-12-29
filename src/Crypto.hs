module Crypto where

import Crypto.Internal (makeKey, Context(..), myPack, myUnpack, Key(..))
import qualified Crypto.Blowfish as Blowfish

-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.
-- | create creates a Key 
createKey :: String -> Context
createKey = either (error . show) (\(Key x) -> either (error.show) id $ Blowfish.initBlowfish x) . makeKey

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
encrypt :: Context -> String -> String
encrypt ctx = myPack . Blowfish.encrypt ctx . myUnpack

decrypt :: Context -> String -> String 
decrypt ctx = myPack . Blowfish.decrypt ctx . myUnpack 

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
encryptMakeKey :: String -> String -> String
encryptMakeKey key = myPack . Blowfish.encrypt ctx . myUnpack
  where ctx = createKey key

decryptMakeKey :: String -> String -> String 
decryptMakeKey key = myPack . Blowfish.decrypt ctx . myUnpack 
  where ctx = createKey key

