module Crypto where

import Crypto.Blowfish (BlowfishContext(..))
import Crypto.Internal (makeKey, myPack, myUnpack, Key(..))
import qualified Crypto.Blowfish as Blowfish

-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.
-- | create creates a Key 
createContext :: String -> BlowfishContext
createContext = (let f (Key x) = either (error.show) id $ Blowfish.initBlowfish x in f) . makeKey

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
encrypt :: BlowfishContext -> String -> String
encrypt ctx = myPack . Blowfish.encrypt ctx . myUnpack

decrypt :: BlowfishContext -> String -> String 
decrypt ctx = myPack . Blowfish.decrypt ctx . myUnpack 

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
encryptMakeKey :: String -> String -> String
encryptMakeKey key = myPack . Blowfish.encrypt ctx . myUnpack
  where ctx = createContext key

decryptMakeKey :: String -> String -> String 
decryptMakeKey key = myPack . Blowfish.decrypt ctx . myUnpack 
  where ctx = createContext key

