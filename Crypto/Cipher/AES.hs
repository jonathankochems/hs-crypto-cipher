module Crypto.Cipher.AES
	( Key
	, encrypt
	, decrypt
	, initKey128
	-- * those key sizes are not actually working right now.
	, initKey192
	, initKey256
	) where

import Data.Word
import Data.Vector.Unboxed (Vector, (//))
import qualified Data.Vector.Unboxed as V
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Control.Monad.State.Strict

newtype Key = Key (Vector Word8)
	deriving (Show,Eq)

type AESState = Vector Word8

{- | encrypt with the key a bytestring and returns the encrypted bytestring -}
encrypt :: Key -> B.ByteString -> B.ByteString
encrypt key b
	| B.length b `mod` 16 == 0 = B.concat $ doChunks (coreEncrypt key) b
	| otherwise                = error "wrong length"

{- | decrypt with the key a bytestring and returns the encrypted bytestring -}
decrypt :: Key -> B.ByteString -> B.ByteString
decrypt key b
	| B.length b `mod` 16 == 0 = B.concat $ doChunks (coreDecrypt key) b
	| otherwise                = error "wrong length"

doChunks :: (B.ByteString -> B.ByteString) -> B.ByteString -> [B.ByteString]
doChunks f b =
	let (x, rest) = B.splitAt 16 b in
	if B.length rest >= 16
		then f x : doChunks f rest
		else [ f x ]

coreEncrypt :: Key -> ByteString -> ByteString
coreEncrypt key input = swapBlockInv $ aesMain 10 key $ swapBlock input

coreDecrypt :: Key -> ByteString -> ByteString
coreDecrypt key input = swapBlockInv $ aesMainInv 10 key $ swapBlock input

initKey128 :: ByteString -> Either String Key
initKey128 = initKey 16 10

initKey192 :: ByteString -> Either String Key
initKey192 = initKey 24 12

initKey256 :: ByteString -> Either String Key
initKey256 = initKey 32 14

initKey :: Int -> Int -> ByteString -> Either String Key
initKey sz nbr b
	| B.length b == sz = Right $ coreExpandKey nbr (V.generate sz $ B.unsafeIndex b)
	| otherwise        = Left "wrong key size"

aesMain :: Int -> Key -> AESState -> AESState
aesMain nbr key block = flip execState block $ do
	modify $ addRoundKey key 0

	forM_ [1..nbr-1] $ \i -> do
		modify shiftRows
		modify mixColumns
		modify $ addRoundKey key i

        modify shiftRows
        modify $ addRoundKey key nbr

aesMainInv :: Int -> Key -> AESState -> AESState
aesMainInv nbr key block = flip execState block $ do
	modify $ addRoundKey key nbr
        
	forM_ (reverse [1..nbr-1]) $ \i -> do
		modify shiftRowsInv
		modify $ addRoundKey key i
		modify mixColumnsInv

        modify shiftRowsInv
        modify $ addRoundKey key 0

{- 0 -> 0, 1 -> 4, ... -}
swapIndexes :: Vector Int
swapIndexes = V.fromList [ 0, 4, 8, 12, 1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15 ]
swapIndex :: Int -> Int
swapIndex i = V.unsafeIndex swapIndexes i

coreExpandKey :: Int -> Vector Word8 -> Key
coreExpandKey nbr vkey = Key (V.concat (ek0 : ekN))
	where
		ek0 = vkey
		ekN = reverse $ snd $ foldl generateFold (ek0, []) [1..nbr]

		generateFold (prevk, accK) it = let nk = generate prevk it in (nk, nk : accK)
		generate prevk it =
			let v0 = cR0 it (V.unsafeIndex prevk 12)
			                (V.unsafeIndex prevk 13)
			                (V.unsafeIndex prevk 14)
			                (V.unsafeIndex prevk 15) in
			let (e0,e1,e2,e3) = xorVector prevk 0 v0 in
			let (e4,e5,e6,e7) = xorVector prevk 4 (e0,e1,e2,e3) in
			let (e8,e9,e10,e11) = xorVector prevk 8 (e4,e5,e6,e7) in
			let (e12,e13,e14,e15) = xorVector prevk 12 (e8,e9,e10,e11) in
			V.fromList [e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15]

		xorVector k i (t0,t1,t2,t3) =
			( V.unsafeIndex k (i+0) `xor` t0
			, V.unsafeIndex k (i+1) `xor` t1
			, V.unsafeIndex k (i+2) `xor` t2
			, V.unsafeIndex k (i+3) `xor` t3
			)

		cR0 it r0 r1 r2 r3 =
			(mSbox r1 `xor` mRcon it, mSbox r2, mSbox r3, mSbox r0)

shiftRows :: AESState -> AESState
shiftRows ost =
	let st = V.map mSbox ost in
	st // [ (7, V.unsafeIndex st 4), (4, V.unsafeIndex st 5), (5, V.unsafeIndex st 6), (6, V.unsafeIndex st 7)
	      , (10, V.unsafeIndex st 8), (11, V.unsafeIndex st 9), (8, V.unsafeIndex st 10), (9, V.unsafeIndex st 11)
	      , (13, V.unsafeIndex st 12), (14, V.unsafeIndex st 13), (15, V.unsafeIndex st 14), (12, V.unsafeIndex st 15)
	      ]

addRoundKey :: Key -> Int -> AESState -> AESState
addRoundKey (Key key) i = V.zipWith (\v1 v2 -> v1 `xor` v2) rk
	where
		rk = V.generate 16 (\n -> V.unsafeIndex key (16 * i + swapIndex n))


mixColumns :: AESState -> AESState
mixColumns state =
	let (state0, state4, state8, state12)  = pr 0 in
	let (state1, state5, state9, state13)  = pr 1 in
	let (state2, state6, state10, state14) = pr 2 in
	let (state3, state7, state11, state15) = pr 3 in
	state //
		[ (0,state0), (1,state1), (2,state2), (3,state3)
		, (4,state4), (5,state5), (6,state6), (7,state7)
		, (8,state8), (9,state9), (10,state10), (11,state11)
		, (12,state12), (13,state13), (14,state14), (15,state15)
		]
	where
		pr i =
			let cpy0 = V.unsafeIndex state (0 * 4 + i) in
			let cpy1 = V.unsafeIndex state (1 * 4 + i) in
			let cpy2 = V.unsafeIndex state (2 * 4 + i) in
			let cpy3 = V.unsafeIndex state (3 * 4 + i) in

			(gm2 cpy0 `xor` gm1 cpy3 `xor` gm1 cpy2 `xor` gm3 cpy1
			,gm2 cpy1 `xor` gm1 cpy0 `xor` gm1 cpy3 `xor` gm3 cpy2
			,gm2 cpy2 `xor` gm1 cpy1 `xor` gm1 cpy0 `xor` gm3 cpy3
			,gm2 cpy3 `xor` gm1 cpy2 `xor` gm1 cpy1 `xor` gm3 cpy0)
		gm1 a = a
		gm2 a = V.unsafeIndex gmtab2 $ fromIntegral a
		gm3 a = V.unsafeIndex gmtab3 $ fromIntegral a

shiftRowsInv :: AESState -> AESState
shiftRowsInv st =
	let nst = st //
		[ (5, V.unsafeIndex st 4), (6, V.unsafeIndex st 5), (7, V.unsafeIndex st 6), (4, V.unsafeIndex st 7)
		, (10, V.unsafeIndex st 8), (11, V.unsafeIndex st 9), (8, V.unsafeIndex st 10), (9, V.unsafeIndex st 11)
		, (15, V.unsafeIndex st 12), (12, V.unsafeIndex st 13), (13, V.unsafeIndex st 14), (14, V.unsafeIndex st 15)
		] in
	V.map mRsbox nst

mixColumnsInv :: AESState -> AESState
mixColumnsInv state =
	let (state0, state4, state8, state12)  = pr 0 in
	let (state1, state5, state9, state13)  = pr 1 in
	let (state2, state6, state10, state14) = pr 2 in
	let (state3, state7, state11, state15) = pr 3 in
	state //
		[ (0,state0), (1,state1), (2,state2), (3,state3)
		, (4,state4), (5,state5), (6,state6), (7,state7)
		, (8,state8), (9,state9), (10,state10), (11,state11)
		, (12,state12), (13,state13), (14,state14), (15,state15)
		]
	where
		pr i = 
			let cpy0 = V.unsafeIndex state (0 * 4 + i) in
			let cpy1 = V.unsafeIndex state (1 * 4 + i) in
			let cpy2 = V.unsafeIndex state (2 * 4 + i) in
			let cpy3 = V.unsafeIndex state (3 * 4 + i) in

			(gm14 cpy0 `xor` gm9 cpy3 `xor` gm13 cpy2 `xor` gm11 cpy1
			,gm14 cpy1 `xor` gm9 cpy0 `xor` gm13 cpy3 `xor` gm11 cpy2
			,gm14 cpy2 `xor` gm9 cpy1 `xor` gm13 cpy0 `xor` gm11 cpy3
			,gm14 cpy3 `xor` gm9 cpy2 `xor` gm13 cpy1 `xor` gm11 cpy0)
		gm14 a = V.unsafeIndex gmtab14 $ fromIntegral a
		gm13 a = V.unsafeIndex gmtab13 $ fromIntegral a
		gm11 a = V.unsafeIndex gmtab11 $ fromIntegral a
		gm9 a  = V.unsafeIndex gmtab9 $ fromIntegral a

swapBlock :: ByteString -> AESState
swapBlock b = V.generate 16 (\i -> B.unsafeIndex b $ swapIndex i)

swapBlockInv :: AESState -> ByteString
swapBlockInv v = B.pack $ map (V.unsafeIndex v . swapIndex) [0..15]

mSbox :: Word8 -> Word8
mSbox = V.unsafeIndex sbox . fromIntegral

mRsbox :: Word8 -> Word8
mRsbox = V.unsafeIndex rsbox . fromIntegral

mRcon :: Int -> Word8
mRcon i = V.unsafeIndex rcon (i `mod` len)
	where len = V.length rcon

sbox :: Vector Word8
sbox = V.fromList 
	[ 0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5
	, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76
	, 0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0
	, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0
	, 0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc
	, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15
	, 0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a
	, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75
	, 0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0
	, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84
	, 0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b
	, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf
	, 0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85
	, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8
	, 0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5
	, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2
	, 0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17
	, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73
	, 0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88
	, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb
	, 0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c
	, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79
	, 0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9
	, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08
	, 0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6
	, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a
	, 0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e
	, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e
	, 0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94
	, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf
	, 0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68
	, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
	]

rsbox :: Vector Word8
rsbox = V.fromList
	[ 0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38
	, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb
	, 0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87
	, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb
	, 0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d
	, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e
	, 0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2
	, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25
	, 0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16
	, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92
	, 0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda
	, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84
	, 0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a
	, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06
	, 0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02
	, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b
	, 0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea
	, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73
	, 0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85
	, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e
	, 0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89
	, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b
	, 0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20
	, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4
	, 0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31
	, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f
	, 0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d
	, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef
	, 0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0
	, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61
	, 0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26
	, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d
	]

rcon :: Vector Word8
rcon = V.fromList
	[ 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40
	, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a
	, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a
	, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39
	, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25
	, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a
	, 0x74, 0xe8, 0xcb
	]

gmtab2, gmtab3, gmtab9, gmtab11, gmtab13, gmtab14 :: Vector Word8
gmtab2 = V.fromList
	[ 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x0c, 0x0e
	, 0x10, 0x12, 0x14, 0x16, 0x18, 0x1a, 0x1c, 0x1e
	, 0x20, 0x22, 0x24, 0x26, 0x28, 0x2a, 0x2c, 0x2e
	, 0x30, 0x32, 0x34, 0x36, 0x38, 0x3a, 0x3c, 0x3e
	, 0x40, 0x42, 0x44, 0x46, 0x48, 0x4a, 0x4c, 0x4e
	, 0x50, 0x52, 0x54, 0x56, 0x58, 0x5a, 0x5c, 0x5e
	, 0x60, 0x62, 0x64, 0x66, 0x68, 0x6a, 0x6c, 0x6e
	, 0x70, 0x72, 0x74, 0x76, 0x78, 0x7a, 0x7c, 0x7e
	, 0x80, 0x82, 0x84, 0x86, 0x88, 0x8a, 0x8c, 0x8e
	, 0x90, 0x92, 0x94, 0x96, 0x98, 0x9a, 0x9c, 0x9e
	, 0xa0, 0xa2, 0xa4, 0xa6, 0xa8, 0xaa, 0xac, 0xae
	, 0xb0, 0xb2, 0xb4, 0xb6, 0xb8, 0xba, 0xbc, 0xbe
	, 0xc0, 0xc2, 0xc4, 0xc6, 0xc8, 0xca, 0xcc, 0xce
	, 0xd0, 0xd2, 0xd4, 0xd6, 0xd8, 0xda, 0xdc, 0xde
	, 0xe0, 0xe2, 0xe4, 0xe6, 0xe8, 0xea, 0xec, 0xee
	, 0xf0, 0xf2, 0xf4, 0xf6, 0xf8, 0xfa, 0xfc, 0xfe
	, 0x1b, 0x19, 0x1f, 0x1d, 0x13, 0x11, 0x17, 0x15
	, 0x0b, 0x09, 0x0f, 0x0d, 0x03, 0x01, 0x07, 0x05
	, 0x3b, 0x39, 0x3f, 0x3d, 0x33, 0x31, 0x37, 0x35
	, 0x2b, 0x29, 0x2f, 0x2d, 0x23, 0x21, 0x27, 0x25
	, 0x5b, 0x59, 0x5f, 0x5d, 0x53, 0x51, 0x57, 0x55
	, 0x4b, 0x49, 0x4f, 0x4d, 0x43, 0x41, 0x47, 0x45
	, 0x7b, 0x79, 0x7f, 0x7d, 0x73, 0x71, 0x77, 0x75
	, 0x6b, 0x69, 0x6f, 0x6d, 0x63, 0x61, 0x67, 0x65
	, 0x9b, 0x99, 0x9f, 0x9d, 0x93, 0x91, 0x97, 0x95
	, 0x8b, 0x89, 0x8f, 0x8d, 0x83, 0x81, 0x87, 0x85
	, 0xbb, 0xb9, 0xbf, 0xbd, 0xb3, 0xb1, 0xb7, 0xb5
	, 0xab, 0xa9, 0xaf, 0xad, 0xa3, 0xa1, 0xa7, 0xa5
	, 0xdb, 0xd9, 0xdf, 0xdd, 0xd3, 0xd1, 0xd7, 0xd5
	, 0xcb, 0xc9, 0xcf, 0xcd, 0xc3, 0xc1, 0xc7, 0xc5
	, 0xfb, 0xf9, 0xff, 0xfd, 0xf3, 0xf1, 0xf7, 0xf5
	, 0xeb, 0xe9, 0xef, 0xed, 0xe3, 0xe1, 0xe7, 0xe5
	]

gmtab3 = V.fromList
	[ 0x00, 0x03, 0x06, 0x05, 0x0c, 0x0f, 0x0a, 0x09
	, 0x18, 0x1b, 0x1e, 0x1d, 0x14, 0x17, 0x12, 0x11
	, 0x30, 0x33, 0x36, 0x35, 0x3c, 0x3f, 0x3a, 0x39
	, 0x28, 0x2b, 0x2e, 0x2d, 0x24, 0x27, 0x22, 0x21
	, 0x60, 0x63, 0x66, 0x65, 0x6c, 0x6f, 0x6a, 0x69
	, 0x78, 0x7b, 0x7e, 0x7d, 0x74, 0x77, 0x72, 0x71
	, 0x50, 0x53, 0x56, 0x55, 0x5c, 0x5f, 0x5a, 0x59
	, 0x48, 0x4b, 0x4e, 0x4d, 0x44, 0x47, 0x42, 0x41
	, 0xc0, 0xc3, 0xc6, 0xc5, 0xcc, 0xcf, 0xca, 0xc9
	, 0xd8, 0xdb, 0xde, 0xdd, 0xd4, 0xd7, 0xd2, 0xd1
	, 0xf0, 0xf3, 0xf6, 0xf5, 0xfc, 0xff, 0xfa, 0xf9
	, 0xe8, 0xeb, 0xee, 0xed, 0xe4, 0xe7, 0xe2, 0xe1
	, 0xa0, 0xa3, 0xa6, 0xa5, 0xac, 0xaf, 0xaa, 0xa9
	, 0xb8, 0xbb, 0xbe, 0xbd, 0xb4, 0xb7, 0xb2, 0xb1
	, 0x90, 0x93, 0x96, 0x95, 0x9c, 0x9f, 0x9a, 0x99
	, 0x88, 0x8b, 0x8e, 0x8d, 0x84, 0x87, 0x82, 0x81
	, 0x9b, 0x98, 0x9d, 0x9e, 0x97, 0x94, 0x91, 0x92
	, 0x83, 0x80, 0x85, 0x86, 0x8f, 0x8c, 0x89, 0x8a
	, 0xab, 0xa8, 0xad, 0xae, 0xa7, 0xa4, 0xa1, 0xa2
	, 0xb3, 0xb0, 0xb5, 0xb6, 0xbf, 0xbc, 0xb9, 0xba
	, 0xfb, 0xf8, 0xfd, 0xfe, 0xf7, 0xf4, 0xf1, 0xf2
	, 0xe3, 0xe0, 0xe5, 0xe6, 0xef, 0xec, 0xe9, 0xea
	, 0xcb, 0xc8, 0xcd, 0xce, 0xc7, 0xc4, 0xc1, 0xc2
	, 0xd3, 0xd0, 0xd5, 0xd6, 0xdf, 0xdc, 0xd9, 0xda
	, 0x5b, 0x58, 0x5d, 0x5e, 0x57, 0x54, 0x51, 0x52
	, 0x43, 0x40, 0x45, 0x46, 0x4f, 0x4c, 0x49, 0x4a
	, 0x6b, 0x68, 0x6d, 0x6e, 0x67, 0x64, 0x61, 0x62
	, 0x73, 0x70, 0x75, 0x76, 0x7f, 0x7c, 0x79, 0x7a
	, 0x3b, 0x38, 0x3d, 0x3e, 0x37, 0x34, 0x31, 0x32
	, 0x23, 0x20, 0x25, 0x26, 0x2f, 0x2c, 0x29, 0x2a
	, 0x0b, 0x08, 0x0d, 0x0e, 0x07, 0x04, 0x01, 0x02
	, 0x13, 0x10, 0x15, 0x16, 0x1f, 0x1c, 0x19, 0x1a
	]

gmtab9 = V.fromList
	[ 0x00, 0x09, 0x12, 0x1b, 0x24, 0x2d, 0x36, 0x3f
	, 0x48, 0x41, 0x5a, 0x53, 0x6c, 0x65, 0x7e, 0x77
	, 0x90, 0x99, 0x82, 0x8b, 0xb4, 0xbd, 0xa6, 0xaf
	, 0xd8, 0xd1, 0xca, 0xc3, 0xfc, 0xf5, 0xee, 0xe7
	, 0x3b, 0x32, 0x29, 0x20, 0x1f, 0x16, 0x0d, 0x04
	, 0x73, 0x7a, 0x61, 0x68, 0x57, 0x5e, 0x45, 0x4c
	, 0xab, 0xa2, 0xb9, 0xb0, 0x8f, 0x86, 0x9d, 0x94
	, 0xe3, 0xea, 0xf1, 0xf8, 0xc7, 0xce, 0xd5, 0xdc
	, 0x76, 0x7f, 0x64, 0x6d, 0x52, 0x5b, 0x40, 0x49
	, 0x3e, 0x37, 0x2c, 0x25, 0x1a, 0x13, 0x08, 0x01
	, 0xe6, 0xef, 0xf4, 0xfd, 0xc2, 0xcb, 0xd0, 0xd9
	, 0xae, 0xa7, 0xbc, 0xb5, 0x8a, 0x83, 0x98, 0x91
	, 0x4d, 0x44, 0x5f, 0x56, 0x69, 0x60, 0x7b, 0x72
	, 0x05, 0x0c, 0x17, 0x1e, 0x21, 0x28, 0x33, 0x3a
	, 0xdd, 0xd4, 0xcf, 0xc6, 0xf9, 0xf0, 0xeb, 0xe2
	, 0x95, 0x9c, 0x87, 0x8e, 0xb1, 0xb8, 0xa3, 0xaa
	, 0xec, 0xe5, 0xfe, 0xf7, 0xc8, 0xc1, 0xda, 0xd3
	, 0xa4, 0xad, 0xb6, 0xbf, 0x80, 0x89, 0x92, 0x9b
	, 0x7c, 0x75, 0x6e, 0x67, 0x58, 0x51, 0x4a, 0x43
	, 0x34, 0x3d, 0x26, 0x2f, 0x10, 0x19, 0x02, 0x0b
	, 0xd7, 0xde, 0xc5, 0xcc, 0xf3, 0xfa, 0xe1, 0xe8
	, 0x9f, 0x96, 0x8d, 0x84, 0xbb, 0xb2, 0xa9, 0xa0
	, 0x47, 0x4e, 0x55, 0x5c, 0x63, 0x6a, 0x71, 0x78
	, 0x0f, 0x06, 0x1d, 0x14, 0x2b, 0x22, 0x39, 0x30
	, 0x9a, 0x93, 0x88, 0x81, 0xbe, 0xb7, 0xac, 0xa5
	, 0xd2, 0xdb, 0xc0, 0xc9, 0xf6, 0xff, 0xe4, 0xed
	, 0x0a, 0x03, 0x18, 0x11, 0x2e, 0x27, 0x3c, 0x35
	, 0x42, 0x4b, 0x50, 0x59, 0x66, 0x6f, 0x74, 0x7d
	, 0xa1, 0xa8, 0xb3, 0xba, 0x85, 0x8c, 0x97, 0x9e
	, 0xe9, 0xe0, 0xfb, 0xf2, 0xcd, 0xc4, 0xdf, 0xd6
	, 0x31, 0x38, 0x23, 0x2a, 0x15, 0x1c, 0x07, 0x0e
	, 0x79, 0x70, 0x6b, 0x62, 0x5d, 0x54, 0x4f, 0x46
	]

gmtab11 = V.fromList
	[ 0x00, 0x0b, 0x16, 0x1d, 0x2c, 0x27, 0x3a, 0x31
	, 0x58, 0x53, 0x4e, 0x45, 0x74, 0x7f, 0x62, 0x69
	, 0xb0, 0xbb, 0xa6, 0xad, 0x9c, 0x97, 0x8a, 0x81
	, 0xe8, 0xe3, 0xfe, 0xf5, 0xc4, 0xcf, 0xd2, 0xd9
	, 0x7b, 0x70, 0x6d, 0x66, 0x57, 0x5c, 0x41, 0x4a
	, 0x23, 0x28, 0x35, 0x3e, 0x0f, 0x04, 0x19, 0x12
	, 0xcb, 0xc0, 0xdd, 0xd6, 0xe7, 0xec, 0xf1, 0xfa
	, 0x93, 0x98, 0x85, 0x8e, 0xbf, 0xb4, 0xa9, 0xa2
	, 0xf6, 0xfd, 0xe0, 0xeb, 0xda, 0xd1, 0xcc, 0xc7
	, 0xae, 0xa5, 0xb8, 0xb3, 0x82, 0x89, 0x94, 0x9f
	, 0x46, 0x4d, 0x50, 0x5b, 0x6a, 0x61, 0x7c, 0x77
	, 0x1e, 0x15, 0x08, 0x03, 0x32, 0x39, 0x24, 0x2f
	, 0x8d, 0x86, 0x9b, 0x90, 0xa1, 0xaa, 0xb7, 0xbc
	, 0xd5, 0xde, 0xc3, 0xc8, 0xf9, 0xf2, 0xef, 0xe4
	, 0x3d, 0x36, 0x2b, 0x20, 0x11, 0x1a, 0x07, 0x0c
	, 0x65, 0x6e, 0x73, 0x78, 0x49, 0x42, 0x5f, 0x54
	, 0xf7, 0xfc, 0xe1, 0xea, 0xdb, 0xd0, 0xcd, 0xc6
	, 0xaf, 0xa4, 0xb9, 0xb2, 0x83, 0x88, 0x95, 0x9e
	, 0x47, 0x4c, 0x51, 0x5a, 0x6b, 0x60, 0x7d, 0x76
	, 0x1f, 0x14, 0x09, 0x02, 0x33, 0x38, 0x25, 0x2e
	, 0x8c, 0x87, 0x9a, 0x91, 0xa0, 0xab, 0xb6, 0xbd
	, 0xd4, 0xdf, 0xc2, 0xc9, 0xf8, 0xf3, 0xee, 0xe5
	, 0x3c, 0x37, 0x2a, 0x21, 0x10, 0x1b, 0x06, 0x0d
	, 0x64, 0x6f, 0x72, 0x79, 0x48, 0x43, 0x5e, 0x55
	, 0x01, 0x0a, 0x17, 0x1c, 0x2d, 0x26, 0x3b, 0x30
	, 0x59, 0x52, 0x4f, 0x44, 0x75, 0x7e, 0x63, 0x68
	, 0xb1, 0xba, 0xa7, 0xac, 0x9d, 0x96, 0x8b, 0x80
	, 0xe9, 0xe2, 0xff, 0xf4, 0xc5, 0xce, 0xd3, 0xd8
	, 0x7a, 0x71, 0x6c, 0x67, 0x56, 0x5d, 0x40, 0x4b
	, 0x22, 0x29, 0x34, 0x3f, 0x0e, 0x05, 0x18, 0x13
	, 0xca, 0xc1, 0xdc, 0xd7, 0xe6, 0xed, 0xf0, 0xfb
	, 0x92, 0x99, 0x84, 0x8f, 0xbe, 0xb5, 0xa8, 0xa3
	]

gmtab13 = V.fromList
	[ 0x00, 0x0d, 0x1a, 0x17, 0x34, 0x39, 0x2e, 0x23
	, 0x68, 0x65, 0x72, 0x7f, 0x5c, 0x51, 0x46, 0x4b
	, 0xd0, 0xdd, 0xca, 0xc7, 0xe4, 0xe9, 0xfe, 0xf3
	, 0xb8, 0xb5, 0xa2, 0xaf, 0x8c, 0x81, 0x96, 0x9b
	, 0xbb, 0xb6, 0xa1, 0xac, 0x8f, 0x82, 0x95, 0x98
	, 0xd3, 0xde, 0xc9, 0xc4, 0xe7, 0xea, 0xfd, 0xf0
	, 0x6b, 0x66, 0x71, 0x7c, 0x5f, 0x52, 0x45, 0x48
	, 0x03, 0x0e, 0x19, 0x14, 0x37, 0x3a, 0x2d, 0x20
	, 0x6d, 0x60, 0x77, 0x7a, 0x59, 0x54, 0x43, 0x4e
	, 0x05, 0x08, 0x1f, 0x12, 0x31, 0x3c, 0x2b, 0x26
	, 0xbd, 0xb0, 0xa7, 0xaa, 0x89, 0x84, 0x93, 0x9e
	, 0xd5, 0xd8, 0xcf, 0xc2, 0xe1, 0xec, 0xfb, 0xf6
	, 0xd6, 0xdb, 0xcc, 0xc1, 0xe2, 0xef, 0xf8, 0xf5
	, 0xbe, 0xb3, 0xa4, 0xa9, 0x8a, 0x87, 0x90, 0x9d
	, 0x06, 0x0b, 0x1c, 0x11, 0x32, 0x3f, 0x28, 0x25
	, 0x6e, 0x63, 0x74, 0x79, 0x5a, 0x57, 0x40, 0x4d
	, 0xda, 0xd7, 0xc0, 0xcd, 0xee, 0xe3, 0xf4, 0xf9
	, 0xb2, 0xbf, 0xa8, 0xa5, 0x86, 0x8b, 0x9c, 0x91
	, 0x0a, 0x07, 0x10, 0x1d, 0x3e, 0x33, 0x24, 0x29
	, 0x62, 0x6f, 0x78, 0x75, 0x56, 0x5b, 0x4c, 0x41
	, 0x61, 0x6c, 0x7b, 0x76, 0x55, 0x58, 0x4f, 0x42
	, 0x09, 0x04, 0x13, 0x1e, 0x3d, 0x30, 0x27, 0x2a
	, 0xb1, 0xbc, 0xab, 0xa6, 0x85, 0x88, 0x9f, 0x92
	, 0xd9, 0xd4, 0xc3, 0xce, 0xed, 0xe0, 0xf7, 0xfa
	, 0xb7, 0xba, 0xad, 0xa0, 0x83, 0x8e, 0x99, 0x94
	, 0xdf, 0xd2, 0xc5, 0xc8, 0xeb, 0xe6, 0xf1, 0xfc
	, 0x67, 0x6a, 0x7d, 0x70, 0x53, 0x5e, 0x49, 0x44
	, 0x0f, 0x02, 0x15, 0x18, 0x3b, 0x36, 0x21, 0x2c
	, 0x0c, 0x01, 0x16, 0x1b, 0x38, 0x35, 0x22, 0x2f
	, 0x64, 0x69, 0x7e, 0x73, 0x50, 0x5d, 0x4a, 0x47
	, 0xdc, 0xd1, 0xc6, 0xcb, 0xe8, 0xe5, 0xf2, 0xff
	, 0xb4, 0xb9, 0xae, 0xa3, 0x80, 0x8d, 0x9a, 0x97
	]

gmtab14 = V.fromList
	[ 0x00, 0x0e, 0x1c, 0x12, 0x38, 0x36, 0x24, 0x2a
	, 0x70, 0x7e, 0x6c, 0x62, 0x48, 0x46, 0x54, 0x5a
	, 0xe0, 0xee, 0xfc, 0xf2, 0xd8, 0xd6, 0xc4, 0xca
	, 0x90, 0x9e, 0x8c, 0x82, 0xa8, 0xa6, 0xb4, 0xba
	, 0xdb, 0xd5, 0xc7, 0xc9, 0xe3, 0xed, 0xff, 0xf1
	, 0xab, 0xa5, 0xb7, 0xb9, 0x93, 0x9d, 0x8f, 0x81
	, 0x3b, 0x35, 0x27, 0x29, 0x03, 0x0d, 0x1f, 0x11
	, 0x4b, 0x45, 0x57, 0x59, 0x73, 0x7d, 0x6f, 0x61
	, 0xad, 0xa3, 0xb1, 0xbf, 0x95, 0x9b, 0x89, 0x87
	, 0xdd, 0xd3, 0xc1, 0xcf, 0xe5, 0xeb, 0xf9, 0xf7
	, 0x4d, 0x43, 0x51, 0x5f, 0x75, 0x7b, 0x69, 0x67
	, 0x3d, 0x33, 0x21, 0x2f, 0x05, 0x0b, 0x19, 0x17
	, 0x76, 0x78, 0x6a, 0x64, 0x4e, 0x40, 0x52, 0x5c
	, 0x06, 0x08, 0x1a, 0x14, 0x3e, 0x30, 0x22, 0x2c
	, 0x96, 0x98, 0x8a, 0x84, 0xae, 0xa0, 0xb2, 0xbc
	, 0xe6, 0xe8, 0xfa, 0xf4, 0xde, 0xd0, 0xc2, 0xcc
	, 0x41, 0x4f, 0x5d, 0x53, 0x79, 0x77, 0x65, 0x6b
	, 0x31, 0x3f, 0x2d, 0x23, 0x09, 0x07, 0x15, 0x1b
	, 0xa1, 0xaf, 0xbd, 0xb3, 0x99, 0x97, 0x85, 0x8b
	, 0xd1, 0xdf, 0xcd, 0xc3, 0xe9, 0xe7, 0xf5, 0xfb
	, 0x9a, 0x94, 0x86, 0x88, 0xa2, 0xac, 0xbe, 0xb0
	, 0xea, 0xe4, 0xf6, 0xf8, 0xd2, 0xdc, 0xce, 0xc0
	, 0x7a, 0x74, 0x66, 0x68, 0x42, 0x4c, 0x5e, 0x50
	, 0x0a, 0x04, 0x16, 0x18, 0x32, 0x3c, 0x2e, 0x20
	, 0xec, 0xe2, 0xf0, 0xfe, 0xd4, 0xda, 0xc8, 0xc6
	, 0x9c, 0x92, 0x80, 0x8e, 0xa4, 0xaa, 0xb8, 0xb6
	, 0x0c, 0x02, 0x10, 0x1e, 0x34, 0x3a, 0x28, 0x26
	, 0x7c, 0x72, 0x60, 0x6e, 0x44, 0x4a, 0x58, 0x56
	, 0x37, 0x39, 0x2b, 0x25, 0x0f, 0x01, 0x13, 0x1d
	, 0x47, 0x49, 0x5b, 0x55, 0x7f, 0x71, 0x63, 0x6d
	, 0xd7, 0xd9, 0xcb, 0xc5, 0xef, 0xe1, 0xf3, 0xfd
	, 0xa7, 0xa9, 0xbb, 0xb5, 0x9f, 0x91, 0x83, 0x8d
	]
