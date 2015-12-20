module CryptoCipherSpec (main, spec) where
 
import Test.Hspec
import Test.QuickCheck

import Data.Char(isPrint)

import Data.ByteString.Char8 hiding (all, take, length)

import Crypto 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do describe "bla"  $ it "should be bla" $ do
            let key       = "testfkjshfksjahdkj"
                message1  = "testmessage asdkhaskjdhasjhgdakjshgdkajshdgkajhsgdkajhsg" 
                cmessage1 = "\162\198\EOT6\173O\211\180B\SYNp3\134\142\196\227[0^`z%\v*\f\142G1!\219a\251\DC1\144\167/\130\"D\145\176\172h\152E\166;%\159pP\199?\225/\183"
                message2  = "rumtitumatataat blweijdasdeeswig" 
                cmessage2 = "\DC4=\174\ETXH[TF\157\ETXx\t\138^\197*.\194\&9\241K\164?\196\134VMo\138k\195\164"
            cryptKey   key message1  `shouldBe` cmessage1
            decryptKey key cmessage1 `shouldBe` message1
            cryptKey   key message2  `shouldBe` cmessage2
            decryptKey key cmessage2 `shouldBe` message2
          describe "spec" $ it "should do something" $ do
            let message = "testmessage asdkhaskjdhasjhgdakjshgdkajshdgkajhsgdkajhsg"
            let e = cryptKey "testfkjshfksjahdkj" message
                d = decryptKey "testfkjshfksjahdkj" e
            (length message `mod` 8) `shouldBe` 0
            d `shouldBe` message 
          describe "encrypt" $ it "should do something" $ property $ 
            forAll message $ 
              \message' -> 
                 do let message = pad message'
                        e = cryptKey "testfkjshfksjahdkj" message
                        d = decryptKey "testfkjshfksjahdkj" e
                    (length message `mod` 8) `shouldBe` 0
                    d `shouldBe` message  
  where pad s = s ++ take (8-length s `mod` 8) [' ' | x <- [1..] ] 
 
message :: Gen String
message = do
    l <- choose(0,10)
    suchThat (vector l) $ all isPrint
