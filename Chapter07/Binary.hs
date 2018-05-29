module Chapter07.Binary where

import           Data.Char

type Bit = Int

-- Convert a binary number, represented as a list of bits, to an integer
{-
bin2int [1, 1, 0, 1] -- 11
-}
bin2int :: [Bit] -> Int
-- bin2int bits = sum [w * b | (w, b) <- zip weights bits]
--                where weights = iterate (* 2) 1
bin2int = foldr (\x y -> x + 2 * y) 0

-- Convert an integer to a list of bits
{-
int2bin 13 -- [1,0,1,1]
(bin2int . int2bin) 123 -- 123
-}
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- Right-pad a binary number with zeroes to 8 bits - truncates to 8 bits if needed
{-
make8 [1,0,1]                   -- [1,0,1,0,0,0,0,0]
make8 [1,0,1,1,0,1,1,0,1,1,0,1] -- [1,0,1,1,0,1,1,0]
-}
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Encode a string of characters as a list of bits
{-
encode "abc" -- [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
-}
encode :: String -> [Bit]
encode = concatMap $ make8 . int2bin . ord

-- Chop a list of bits into a list of groups of 8 bits
{-
chop8 $ encode "abc" -- [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]
-}
chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- Decode a list of bits into a string
{-
decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] -- "abc"
-}
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- Simulate the transmission of a string of characters as a list of bits
{-
transmit "abc" -- "abc"
-}
transmit :: String -> String
transmit = decode . channel . encode

-- A perfect communication channel - does nothing
channel :: [Bit] -> [Bit]
channel = id
