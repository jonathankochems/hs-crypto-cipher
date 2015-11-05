module CryptoCipherSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Char(isPrint)

import Data.ByteString.Char8 hiding (all, take, length)
import Crypto.Cipher.Types
import Crypto.Cipher.Blowfish
-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.

initC :: ByteString -> Blowfish
initC = either (error . show) cipherInit . makeKey

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
cryptKey key = ecbEncrypt ctx
  where ctx = initC key

decryptKey key = ecbDecrypt ctx
  where ctx = initC key


main :: IO ()
main = hspec spec

spec :: Spec
spec = do describe "spec" $ it "should do something" $ do
            let message = "testmessage asdkhaskjdhasjhgdakjshgdkajshdgkajhsgdkajhsg"
            let e = cryptKey (pack "testfkjshfksjahdkj") $ pack message
                d = decryptKey (pack "testfkjshfksjahdkj") e
            (length message `mod` 8) `shouldBe` 0
            unpack d `shouldBe` message 
          describe "encrypt" $ it "should do something" $ property $ 
            forAll message $ 
              \message' -> 
                 do let message = pad message'
                        e = cryptKey (pack "testfkjshfksjahdkj") $ pack message
                        d = decryptKey (pack "testfkjshfksjahdkj") e
                    (length message `mod` 8) `shouldBe` 0
                    unpack d `shouldBe` message  
  where pad s = s ++ take (8-length s `mod` 8) [' ' | x <- [1..] ] 
 
message :: Gen String
message = do
    l <- choose(0,10)
    suchThat (vector l) $ all isPrint
