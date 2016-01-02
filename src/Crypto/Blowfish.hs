-- |
-- Module      : Crypto.Cipher.Blowfish.Primitive
-- License     : BSD-style
-- Stability   : experimental
-- Portability : Good

-- Crypto.Cipher.Blowfish.Primitive, copyright (c) 2012 Stijn van Drongelen
-- based on: BlowfishAux.hs (C) 2002 HardCore SoftWare, Doug Hoyte
--           (as found in Crypto-4.2.4)

module Crypto.Blowfish
    ( BlowfishContext(..)
    , initBlowfish
    , encrypt
    , decrypt
    ) where

import Data.List as V
import Data.Bits (shiftR, shiftL, (.|.), (.&.), xor)
import Data.Char (ord)
import Data.Word (Word32, Word64, Word8)

type Pbox = [Word32]
type Sbox = [Word32]

(//) :: [a] -> [(Int,a)] -> [a]
(//) xs ys' = go xs ys 0
  where ys = sortBy (let f x y = compare (fst x) (fst y) in f) ys'
        go [] _ _ = []
        go as [] _ = as
        go (a:as) ((i,b):bs) j | i == j     = b : go as bs (j+1)
                               | otherwise  = a : go as ((i,b):bs) (j+1)
generateVec :: Int -> (Int -> a) -> [a]
generateVec x f = map f . take x $ [0..] 

-- | variable keyed blowfish state
data BlowfishContext = BlowfishContext Pbox Sbox Sbox Sbox Sbox
  deriving Show

encrypt, decrypt :: BlowfishContext -> [Int] -> [Int]
encrypt = cipher . selectEncrypt
decrypt = cipher . selectDecrypt

selectEncrypt, selectDecrypt :: BlowfishContext -> (Pbox, BlowfishContext)
selectEncrypt x@(BlowfishContext p _ _ _ _) = (p, x)
selectDecrypt x@(BlowfishContext p _ _ _ _) = (V.reverse p, x)

cipher :: (Pbox, BlowfishContext) -> [Int] -> [Int]
cipher (p, bs) b
    | length b == 0 = []
    | length b `mod` 8 /= 0 = error "invalid data length"
    | otherwise = concat $ doChunks 8 (fromW32Pair . coreCrypto p bs . toW32Pair) b

initBlowfish :: [Int] -> Either String BlowfishContext
initBlowfish b
    | length b > (448 `div` 8) = Left "key too large"
    | length b == 0 = keyFromByteString (replicate (18*4) 0)
    | otherwise = keyFromByteString . take (18*4) $ cycle b

keyFromByteString :: [Int] -> Either String BlowfishContext
keyFromByteString k
    | length k /= (18 * 4) = Left "Incorrect expanded key length."
    | otherwise = Right . bfMakeKey . (let f ws = generateVec 18 (ws!!) in f) . w8tow32 $ map fromIntegral k
  where
    w8tow32 :: [Word8] -> [Word32]
    w8tow32 [] = []
    w8tow32 (a:b:c:d:xs) = ( (fromIntegral a `shiftL` 24) .|.
                             (fromIntegral b `shiftL` 16) .|.
                             (fromIntegral c `shiftL`  8) .|.
                             (fromIntegral d) ) : w8tow32 xs
    w8tow32 _ = error $ "internal error: Crypto.Cipher.Blowfish:keyFromByteString"

coreCrypto :: Pbox -> BlowfishContext -> (Word32, Word32) -> (Word32, Word32)
coreCrypto p bs i = (let f (l,r)= (r `xor` p!!17, l `xor` p!!16) in f)
                  $ V.foldl' (doRound bs) i (V.take 16 p)
  where
    doRound :: BlowfishContext -> (Word32, Word32) -> Word32 -> (Word32, Word32)
    doRound (BlowfishContext _ s0 s1 s2 s3) (l,r) pv =
        let newr = l `xor` pv
            newl = r `xor` (f newr)
        in newl `seq` newr `seq` (newl, newr)
          where
            f   :: Word32 -> Word32
            f t = let a = s0 !! (fromIntegral $ (t `shiftR` 24) .&. 0xff)
                      b = s1 !! (fromIntegral $ (t `shiftR` 16) .&. 0xff)
                      c = s2 !! (fromIntegral $ (t `shiftR` 8) .&. 0xff)
                      d = s3 !! (fromIntegral $ t .&. 0xff)
                  in ((a + b) `xor` c) + d

bfMakeKey :: [Word32] -> BlowfishContext
bfMakeKey k = procKey (0,0) (BlowfishContext (V.zipWith xor k iPbox) iSbox0 iSbox1 iSbox2 iSbox3) 0

procKey :: (Word32, Word32) -> BlowfishContext -> Int -> BlowfishContext
procKey _     tpbf                    1042 = tpbf
procKey (l,r) tpbf@(BlowfishContext p s0 s1 s2 s3)    i = nl `seq` nr `seq` newbf i `seq` i+2 `seq` procKey (nl,nr) (newbf i) (i+2)
  where (nl,nr) = coreCrypto p tpbf (l,r)
        newbf x | x <   18 = (BlowfishContext (p//[(x,nl),(x+1,nr)]) s0 s1 s2 s3)
                | x <  274 = (BlowfishContext p (s0//[(x-18,nl),(x-17,nr)]) s1 s2 s3)
                | x <  530 = (BlowfishContext p s0 (s1//[(x-274,nl),(x-273,nr)]) s2 s3)
                | x <  786 = (BlowfishContext p s0 s1 (s2//[(x-530,nl),(x-529,nr)]) s3)
                | x < 1042 = (BlowfishContext p s0 s1 s2 (s3//[(x-786,nl),(x-785,nr)]))
                | otherwise = error "internal error: Crypto.Cipher.Blowfish:procKey "

doChunks n f b =
    let (x, rest) = splitAt n b in
    if length rest >= n
        then f x : doChunks n f rest
        else [ f x ]

toW32Pair :: [Int] -> (Word32, Word32)
toW32Pair b = let (x1, x2) = splitAt 4 b
                  w1 = decode32be x1
                  w2 = decode32be x2
              in (fromIntegral w1,fromIntegral w2)


fromW32Pair :: (Word32, Word32) -> [Int]
fromW32Pair (w1,w2)
    = let w1' = fromIntegral w1
          w2' = fromIntegral w2
          w = (w1' `shiftL` 32) .|. w2'
      in encode64be w

decode32be :: [Int] -> Int
decode32be s = id $!
    (fromIntegral (s !! 0) * 2^24) +
    (fromIntegral (s !! 1) * 2^16) +
    (fromIntegral (s !! 2) * 2^8 ) +
    (fromIntegral (s !! 3) )


encode64be :: Word64 -> [Int]
encode64be w = map fromIntegral $
                [ (w `shiftR` 56) .&. 0xff
                , (w `shiftR` 48) .&. 0xff
                , (w `shiftR` 40) .&. 0xff
                , (w `shiftR` 32) .&. 0xff
                , (w `shiftR` 24) .&. 0xff
                , (w `shiftR` 16) .&. 0xff
                , (w `shiftR` 8) .&. 0xff
                , w .&. 0xff
                ]

---------- INITIAL S AND P BOXES ARE THE HEXADECIMAL DIGITS OF PI ------------
mkBox :: [Int] -> [Word32]
mkBox = map fromIntegral . mkBox'
    where mkBox' :: [Int] -> [Int]
          mkBox' = map decode32be . doChunks 4 id . map fromIntegral 

iPbox :: Pbox
iPbox = mkBox [36,63,106,136,133,163,8,211,19,25,138,46,3,112,115,68,164,9
              ,56,34,41,159,49,208,8,46,250,152,236,78,108,137,69,40,33,230
              ,56,208,19,119,190,84,102,207,52,233,12,108,192,172,41,183
              ,201,124,80,221,63,132,213,181,181,71,9,23,146,22,213,217
              ,137,121,251,27]

iSbox0 :: Sbox 
iSbox0 = mkBox [209,49,11,166,152,223,181,172,47,253,114,219,208,26,223,183
               ,184,225,175,237,106,38,126,150,186,124,144,69,241,44,127,153
               ,36,161,153,71,179,145,108,247,8,1,242,226,133,142,252,22,99
               ,105,32,216,113,87,78,105,164,88,254,163,244,147,61,126,13,149
               ,116,143,114,142,182,88,113,139,205,88,130,21,74,238,123,84
               ,164,29,194,90,89,181,156,48,213,57,42,242,96,19,197,209,176
               ,35,40,96,133,240,202,65,121,24,184,219,56,239,142,121,220,176
               ,96,58,24,14,108,158,14,139,176,30,138,62,215,21,119,193,189,49
               ,75,39,120,175,47,218,85,96,92,96,230,85,37,243,170,85,171,148
               ,87,72,152,98,99,232,20,64,85,202,57,106,42,171,16,182,180,204
               ,92,52,17,65,232,206,161,84,134,175,124,114,233,147,179,238,20
               ,17,99,111,188,42,43,169,197,93,116,24,49,246,206,92,62,22,155
               ,135,147,30,175,214,186,51,108,36,207,92,122,50,83,129,40,149
               ,134,119,59,143,72,152,107,75,185,175,196,191,232,27,102,40,33
               ,147,97,216,9,204,251,33,169,145,72,124,172,96,93,236,128,50,239
               ,132,93,93,233,133,117,177,220,38,35,2,235,101,27,136,35,137
               ,62,129,211,150,172,197,15,109,111,243,131,244,66,57,46,11,68,130
               ,164,132,32,4,105,200,240,74,158,31,155,94,33,198,104,66,246,233
               ,108,154,103,12,156,97,171,211,136,240,106,81,160,210,216,84,47
               ,104,150,15,167,40,171,81,51,163,110,239,11,108,19,122,59,228,186
               ,59,240,80,126,251,42,152,161,241,101,29,57,175,1,118,102,202,89
               ,62,130,67,14,136,140,238,134,25,69,111,159,180,125,132,165,195
               ,59,139,94,190,224,111,117,216,133,193,32,115,64,26,68,159,86,193
               ,106,166,78,211,170,98,54,63,119,6,27,254,223,114,66,155,2,61,55
               ,208,215,36,208,10,18,72,219,15,234,211,73,241,192,155,7,83,114
               ,201,128,153,27,123,37,212,121,216,246,232,222,247,227,254,80,26
               ,182,121,76,59,151,108,224,189,4,192,6,186,193,169,79,182,64,159
               ,96,196,94,92,158,194,25,106,36,99,104,251,111,175,62,108,83,181
               ,19,57,178,235,59,82,236,111,109,252,81,31,155,48,149,44,204,129
               ,69,68,175,94,189,9,190,227,208,4,222,51,74,253,102,15,40,7,25
               ,46,75,179,192,203,168,87,69,200,116,15,210,11,95,57,185,211,251
               ,219,85,121,192,189,26,96,50,10,214,161,0,198,64,44,114,121,103
               ,159,37,254,251,31,163,204,142,165,233,248,219,50,34,248,60,117
               ,22,223,253,97,107,21,47,80,30,200,173,5,82,171,50,61,181,250
               ,253,35,135,96,83,49,123,72,62,0,223,130,158,92,87,187,202,111,140
               ,160,26,135,86,46,223,23,105,219,213,66,168,246,40,126,255,195,172
               ,103,50,198,140,79,85,115,105,91,39,176,187,202,88,200,225,255,163
               ,93,184,240,17,160,16,250,61,152,253,33,131,184,74,252,181,108,45
               ,209,211,91,154,83,228,121,182,248,69,101,210,142,73,188,75,251
               ,151,144,225,221,242,218,164,203,126,51,98,251,19,65,206,228,198
               ,232,239,32,202,218,54,119,76,1,208,126,158,254,43,241,31,180,149
               ,219,218,77,174,144,145,152,234,173,142,113,107,147,213,160,208,142
               ,209,208,175,199,37,224,142,60,91,47,142,117,148,183,143,246,226,251
               ,242,18,43,100,136,136,184,18,144,13,240,28,79,173,94,160,104,143
               ,195,28,209,207,241,145,179,168,193,173,47,47,34,24,190,14,23,119
               ,234,117,45,254,139,2,31,161,229,160,204,15,181,111,116,232,24,172
               ,243,214,206,137,226,153,180,168,79,224,253,19,224,183,124,196,59
               ,129,210,173,168,217,22,95,162,102,128,149,119,5,147,204,115,20
               ,33,26,20,119,230,173,32,101,119,181,250,134,199,84,66,245,251
               ,157,53,207,235,205,175,12,123,62,137,160,214,65,27,211,174,30
               ,126,73,0,37,14,45,32,113,179,94,34,104,0,187,87,184,224,175,36,100
               ,54,155,240,9,185,30,85,99,145,29,89,223,166,170,120,193,67,137,217
               ,90,83,127,32,125,91,162,2,229,185,197,131,38,3,118,98,149,207,169
               ,17,200,25,104,78,115,74,65,179,71,45,202,123,20,169,74,27,81,0,82
               ,154,83,41,21,214,15,87,63,188,155,198,228,43,96,164,118,129,230,116
               ,0,8,186,111,181,87,27,233,31,242,150,236,107,42,13,217,21,182,99
               ,101,33,231,185,249,182,255,52,5,46,197,133,86,100,83,176,45,93,169
               ,159,143,161,8,186,71,153,110,133,7,106]


iSbox1 :: Sbox
iSbox1 = mkBox [75,122,112,233,181,179,41,68,219,117,9,46,196,25,38,35,173,110,166,176
               ,73,167,223,125,156,238,96,184,143,237,178,102,236,170,140,113,105,154,23,255
               ,86,100,82,108,194,177,158,225,25,54,2,165,117,9,76,41,160,89,19,64
               ,228,24,58,62,63,84,152,154,91,66,157,101,107,143,228,214,153,247,63,214
               ,161,210,156,7,239,232,48,245,77,45,56,230,240,37,93,193,76,221,32,134
               ,132,112,235,38,99,130,233,198,2,30,204,94,9,104,107,63,62,186,239,201
               ,60,151,24,20,107,106,112,161,104,127,53,132,82,160,226,134,183,156,83,5
               ,170,80,7,55,62,7,132,28,127,222,174,92,142,125,68,236,87,22,242,184
               ,176,58,218,55,240,80,12,13,240,28,31,4,2,0,179,255,174,12,245,26
               ,60,181,116,178,37,131,122,88,220,9,33,189,209,145,19,249,124,169,47,246
               ,148,50,71,115,34,245,71,1,58,229,229,129,55,194,218,220,200,181,118,52
               ,154,243,221,167,169,68,97,70,15,208,3,14,236,200,199,62,164,117,30,65
               ,226,56,205,153,59,234,14,47,50,128,187,161,24,62,179,49,78,84,139,56
               ,79,109,185,8,111,66,13,3,246,10,4,191,44,184,18,144,36,151,124,121
               ,86,121,176,114,188,175,137,175,222,154,119,31,217,147,8,16,179,139,174,18
               ,220,207,63,46,85,18,114,31,46,107,113,36,80,26,221,230,159,132,205,135
               ,122,88,71,24,116,8,218,23,188,159,154,188,233,75,125,140,236,122,236,58
               ,219,133,29,250,99,9,67,102,196,100,195,210,239,28,24,71,50,21,217,8
               ,221,67,59,55,36,194,186,22,18,161,77,67,42,101,196,81,80,148,0,2
               ,19,58,228,221,113,223,248,158,16,49,78,85,129,172,119,214,95,17,25,155
               ,4,53,86,241,215,163,199,107,60,17,24,59,89,36,165,9,242,143,230,237
               ,151,241,251,250,158,186,191,44,30,21,60,110,134,227,69,112,234,233,111,177
               ,134,14,94,10,90,62,42,179,119,31,231,28,78,61,6,250,41,101,220,185
               ,153,231,29,15,128,62,137,214,82,102,200,37,46,76,201,120,156,16,179,106
               ,198,21,14,186,148,226,234,120,165,252,60,83,30,10,45,244,242,247,78,167
               ,54,29,43,61,25,57,38,15,25,194,121,96,82,35,167,8,247,19,18,182
               ,235,173,254,110,234,195,31,102,227,188,69,149,166,123,200,131,177,127,55,209
               ,1,140,255,40,195,50,221,239,190,108,90,165,101,88,33,133,104,171,152,2
               ,238,206,165,15,219,47,149,59,42,239,125,173,91,110,47,132,21,33,182,40
               ,41,7,97,112,236,221,71,117,97,159,21,16,19,204,168,48,235,97,189,150
               ,3,52,254,30,170,3,99,207,181,115,92,144,76,112,162,57,213,158,158,11
               ,203,170,222,20,238,204,134,188,96,98,44,167,156,171,92,171,178,243,132,110
               ,100,139,30,175,25,189,240,202,160,35,105,185,101,90,187,80,64,104,90,50
               ,60,42,180,179,49,158,233,213,192,33,184,247,155,84,11,25,135,95,160,153
               ,149,247,153,126,98,61,125,168,248,55,136,154,151,227,45,119,17,237,147,95
               ,22,104,18,129,14,53,136,41,199,230,31,214,150,222,223,161,120,88,186,153
               ,87,245,132,165,27,34,114,99,155,131,195,255,26,194,70,150,205,179,10,235
               ,83,46,48,84,143,217,72,228,109,188,49,40,88,235,242,239,52,198,255,234
               ,254,40,237,97,238,124,60,115,93,74,20,217,232,100,183,227,66,16,93,20
               ,32,62,19,224,69,238,226,182,163,170,171,234,219,108,79,21,250,203,79,208
               ,199,66,244,66,239,106,187,181,101,79,59,29,65,205,33,5,216,30,121,158
               ,134,133,77,199,228,75,71,106,61,129,98,80,207,98,161,242,91,141,38,70
               ,252,136,131,160,193,199,182,163,127,21,36,195,105,203,116,146,71,132,138,11
               ,86,146,178,133,9,91,191,0,173,25,72,157,20,98,177,116,35,130,14,0
               ,88,66,141,42,12,85,245,234,29,173,244,62,35,63,112,97,51,114,240,146
               ,141,147,126,65,214,95,236,241,108,34,59,219,124,222,55,89,203,238,116,96
               ,64,133,242,167,206,119,50,110,166,7,128,132,25,248,80,158,232,239,216,85
               ,97,217,151,53,169,105,167,170,197,12,6,194,90,4,171,252,128,11,202,220
               ,158,68,122,46,195,69,52,132,253,213,103,5,14,30,158,201,219,115,219,211
               ,16,85,136,205,103,95,218,121,227,103,67,64,197,196,52,101,113,62,56,216
               ,61,40,248,158,241,109,255,32,21,62,33,231,143,176,61,74,230,227,159,43
               ,219,131,173,247]

iSbox2 ::Sbox 
iSbox2 = mkBox [233,61,90,104,148,129,64,247,246,76,38,28,148,105,41,52,65,21,32,247
               ,118,2,212,247,188,244,107,46,212,162,0,104,212,8,36,113,51,32,244,106
               ,67,183,212,183,80,0,97,175,30,57,246,46,151,36,69,70,20,33,79,116
               ,191,139,136,64,77,149,252,29,150,181,145,175,112,244,221,211,102,160,47,69
               ,191,188,9,236,3,189,151,133,127,172,109,208,49,203,133,4,150,235,39,179
               ,85,253,57,65,218,37,71,230,171,202,10,154,40,80,120,37,83,4,41,244
               ,10,44,134,218,233,182,109,251,104,220,20,98,215,72,105,0,104,14,192,164
               ,39,161,141,238,79,63,254,162,232,135,173,140,181,140,224,6,122,244,214,182
               ,170,206,30,124,211,55,95,236,206,120,163,153,64,107,42,66,32,254,158,53
               ,217,243,133,185,238,57,215,171,59,18,78,139,29,201,250,247,75,109,24,86
               ,38,163,102,49,234,227,151,178,58,110,250,116,221,91,67,50,104,65,231,247
               ,202,120,32,251,251,10,245,78,216,254,179,151,69,64,86,172,186,72,149,39
               ,85,83,58,58,32,131,141,135,254,107,169,183,208,150,149,75,85,168,103,188
               ,161,21,154,88,204,169,41,99,153,225,219,51,166,42,74,86,63,49,37,249
               ,94,244,126,28,144,41,49,124,253,248,232,2,4,39,47,112,128,187,21,92
               ,5,40,44,227,149,193,21,72,228,198,109,34,72,193,19,63,199,15,134,220
               ,7,249,201,238,65,4,31,15,64,71,121,164,93,136,110,23,50,95,81,235
               ,213,155,192,209,242,188,193,143,65,17,53,100,37,123,120,52,96,42,156,96
               ,223,248,232,163,31,99,108,27,14,18,180,194,2,225,50,158,175,102,79,209
               ,202,209,129,21,107,35,149,224,51,62,146,225,59,36,11,98,238,190,185,34
               ,133,178,162,14,230,186,13,153,222,114,12,140,45,162,247,40,208,18,120,69
               ,149,183,148,253,100,125,8,98,231,204,245,240,84,73,163,111,135,125,72,250
               ,195,157,253,39,243,62,141,30,10,71,99,65,153,46,255,116,58,111,110,171
               ,244,248,253,55,168,18,220,96,161,235,221,248,153,27,225,76,219,110,107,13
               ,198,123,85,16,109,103,44,55,39,101,212,59,220,208,232,4,241,41,13,199
               ,204,0,255,163,181,57,15,146,105,15,237,11,102,123,159,251,206,219,125,156
               ,160,145,207,11,217,21,94,163,187,19,47,136,81,91,173,36,123,148,121,191
               ,118,59,214,235,55,57,46,179,204,17,89,121,128,38,226,151,244,46,49,45
               ,104,66,173,167,198,106,43,59,18,117,76,204,120,46,241,28,106,18,66,55
               ,183,146,81,231,6,161,187,230,75,251,99,80,26,107,16,24,17,202,237,250
               ,61,37,189,216,226,225,195,201,68,66,22,89,10,18,19,134,217,12,236,110
               ,213,171,234,42,100,175,103,78,218,134,168,95,190,191,233,136,100,228,195,254
               ,157,188,128,87,240,247,192,134,96,120,123,248,96,3,96,77,209,253,131,70
               ,246,56,31,176,119,69,174,4,215,54,252,204,131,66,107,51,240,30,171,113
               ,176,128,65,135,60,0,94,95,119,160,87,190,189,232,174,36,85,70,66,153
               ,191,88,46,97,78,88,244,143,242,221,253,162,244,116,239,56,135,137,189,194
               ,83,102,249,195,200,179,142,116,180,117,242,85,70,252,217,185,122,235,38,97
               ,139,29,223,132,132,106,14,121,145,95,149,226,70,110,89,142,32,180,87,112
               ,140,213,85,145,201,2,222,76,185,11,172,225,187,130,5,208,17,168,98,72
               ,117,116,169,158,183,127,25,182,224,169,220,9,102,45,9,161,196,50,70,51
               ,232,90,31,2,9,240,190,140,74,153,160,37,29,110,254,16,26,185,61,29
               ,11,165,164,223,161,134,242,15,40,104,241,105,220,183,218,131,87,57,6,254
               ,161,226,206,155,79,205,127,82,80,17,94,1,167,6,131,250,160,2,181,196
               ,13,230,208,39,154,248,140,39,119,63,134,65,195,96,76,6,97,168,6,181
               ,240,23,122,40,192,245,134,224,0,96,88,170,48,220,125,98,17,230,158,215
               ,35,56,234,99,83,194,221,148,194,194,22,52,187,203,238,86,144,188,182,222
               ,235,252,125,161,206,89,29,118,111,5,228,9,75,124,1,136,57,114,10,61
               ,124,146,124,36,134,227,114,95,114,77,157,185,26,193,91,180,211,158,184,252
               ,237,84,85,120,8,252,165,181,216,61,124,211,77,173,15,196,30,80,239,94
               ,177,97,230,248,162,133,20,217,108,81,19,60,111,213,199,231,86,225,78,196
               ,54,42,191,206,221,198,200,55,215,154,50,52,146,99,130,18,103,14,250,142
               ,64,96,0,224]

iSbox3 :: Sbox
iSbox3 = mkBox [58,57,206,55,211,250,245,207,171,194,119,55,90,197,45,27,92,176,103,158
               ,79,163,55,66,211,130,39,64,153,188,155,190,213,17,142,157,191,15,115,21
               ,214,45,28,126,199,0,196,123,183,140,27,107,33,161,144,69,178,110,177,190
               ,106,54,110,180,87,72,171,47,188,148,110,121,198,163,118,210,101,73,194,200
               ,83,15,248,238,70,141,222,125,213,115,10,29,76,208,77,198,41,57,187,219
               ,169,186,70,80,172,149,38,232,190,94,227,4,161,250,213,240,106,45,81,154
               ,99,239,140,226,154,134,238,34,192,137,194,184,67,36,46,246,165,30,3,170
               ,156,242,208,164,131,192,97,186,155,233,106,77,143,229,21,80,186,100,91,214
               ,40,38,162,249,167,58,58,225,75,169,149,134,239,85,98,233,199,47,239,211
               ,247,82,247,218,63,4,111,105,119,250,10,89,128,228,169,21,135,176,134,1
               ,155,9,230,173,59,62,229,147,233,144,253,90,158,52,215,151,44,240,183,217
               ,2,43,139,81,150,213,172,58,1,125,166,125,209,207,62,214,124,125,45,40
               ,31,159,37,207,173,242,184,155,90,214,180,114,90,136,245,76,224,41,172,113
               ,224,25,165,230,71,176,172,253,237,147,250,155,232,211,196,141,40,59,87,204
               ,248,213,102,41,121,19,46,40,120,95,1,145,237,117,96,85,247,150,14,68
               ,227,211,94,140,21,5,109,212,136,244,109,186,3,161,97,37,5,100,240,189
               ,195,235,158,21,60,144,87,162,151,39,26,236,169,58,7,42,27,63,109,155
               ,30,99,33,245,245,156,102,251,38,220,243,25,117,51,217,40,177,85,253,245
               ,3,86,52,130,138,186,60,187,40,81,119,17,194,10,217,248,171,204,81,103
               ,204,173,146,95,77,232,23,81,56,48,220,142,55,157,88,98,147,32,249,145
               ,234,122,144,194,251,62,123,206,81,33,206,100,119,79,190,50,168,182,227,126
               ,195,41,61,70,72,222,83,105,100,19,230,128,162,174,8,16,221,109,178,36
               ,105,133,45,253,9,7,33,102,179,154,70,10,100,69,192,221,88,108,222,207
               ,28,32,200,174,91,190,247,221,27,88,141,64,204,210,1,127,107,180,227,187
               ,221,162,106,126,58,89,255,69,62,53,10,68,188,180,205,213,114,234,206,168
               ,250,100,132,187,141,102,18,174,191,60,111,71,210,155,228,99,84,47,93,158
               ,174,194,119,27,246,78,99,112,116,14,13,141,231,91,19,87,248,114,22,113
               ,175,83,125,93,64,64,203,8,78,180,226,204,52,210,70,106,1,21,175,132
               ,225,176,4,40,149,152,58,29,6,184,159,180,206,110,160,72,111,63,59,130
               ,53,32,171,130,1,26,29,75,39,114,39,248,97,21,96,177,231,147,63,220
               ,187,58,121,43,52,69,37,189,160,136,57,225,81,206,121,75,47,50,201,183
               ,160,31,186,201,224,28,200,126,188,199,209,246,207,1,17,195,161,232,170,199
               ,26,144,135,73,212,79,189,154,208,218,222,203,213,10,218,56,3,57,195,42
               ,198,145,54,103,141,249,49,124,224,177,43,79,247,158,89,183,67,245,187,58
               ,242,213,25,255,39,217,69,156,191,151,34,44,21,230,252,42,15,145,252,113
               ,155,148,21,37,250,229,147,97,206,182,156,235,194,168,100,89,18,186,168,209
               ,182,193,7,94,227,5,106,12,16,210,80,101,203,3,164,66,224,236,110,14
               ,22,152,219,59,76,152,160,190,50,120,233,100,159,31,149,50,224,211,146,223
               ,211,160,52,43,137,113,242,30,27,10,116,65,75,163,52,140,197,190,113,32
               ,195,118,50,216,223,53,159,141,155,153,47,46,230,11,111,71,15,227,241,29
               ,229,76,218,84,30,218,216,145,206,98,121,207,205,62,126,111,22,24,177,102
               ,253,44,29,5,132,143,210,197,246,251,34,153,245,35,243,87,166,50,118,35
               ,147,168,53,49,86,204,205,2,172,240,129,98,90,117,235,181,110,22,54,151
               ,136,210,115,204,222,150,98,146,129,185,73,208,76,80,144,27,113,198,86,20
               ,230,198,199,189,50,122,20,10,69,225,208,6,195,242,123,154,201,170,83,253
               ,98,168,15,0,187,37,191,226,53,189,210,246,113,18,105,5,178,4,2,34
               ,182,203,207,124,205,118,156,43,83,17,62,192,22,64,227,211,56,171,189,96
               ,37,71,173,240,186,56,32,156,247,70,206,118,119,175,161,197,32,117,96,96
               ,133,203,254,78,138,232,141,216,122,170,249,176,76,249,170,126,25,72,194,92
               ,2,251,138,140,1,195,106,228,214,235,225,249,144,212,248,105,166,92,222,160
               ,63,9,37,45,194,8,230,159,183,78,97,50,206,119,226,91,87,143,223,227
               ,58,195,114,230]