module CryptoCipherSpec (main, spec) where
 
import Test.Hspec
import Test.QuickCheck

import Data.Char(isPrint)

import Data.ByteString.Char8 hiding (all, take, length)
import Crypto.Cipher.Types (ecbEncrypt, ecbDecrypt, cipherInit, makeKey)
import Crypto.Cipher.Blowfish (Blowfish)
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
spec = do describe "bla"  $ it "should be bla" $ do
            let key       = pack "testfkjshfksjahdkj"
                message1  = pack "testmessage asdkhaskjdhasjhgdakjshgdkajshdgkajhsgdkajhsg" 
                cmessage1 = pack "\162\198\EOT6\173O\211\180B\SYNp3\134\142\196\227[0^`z%\v*\f\142G1!\219a\251\DC1\144\167/\130\"D\145\176\172h\152E\166;%\159pP\199?\225/\183"
                message2  = pack "rumtitumatataat blweijdasdeeswig" 
                cmessage2 = pack "\DC4=\174\ETXH[TF\157\ETXx\t\138^\197*.\194\&9\241K\164?\196\134VMo\138k\195\164"
            cryptKey   key message1  `shouldBe` cmessage1
            decryptKey key cmessage1 `shouldBe` message1
            cryptKey   key message2  `shouldBe` cmessage2
            decryptKey key cmessage2 `shouldBe` message2
          describe "spec" $ it "should do something" $ do
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
