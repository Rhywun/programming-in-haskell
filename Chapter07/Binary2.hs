module Chapter07.Binary2 where

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

--
-- New code for Exercises 7 and 8
--

-- Counts the number of items matching `x` in a list
{-
count 1 [1,0,1,1] -- 3
-}
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- Return the parity bit for a list of bits (1 if the count of one's is odd, otherwise 0)
{-
parityBit [1,0,1,1] -- 1
parityBit [1,0,0,1] -- 0
-}
parityBit :: [Bit] -> Bit
parityBit bits = if odd $ count 1 bits then 1 else 0

-- Right-pad with zeroes to 8 bits and add the parity bit
{-
make9 [1,0,1,1] -- [1,0,1,1,0,0,0,0,1]
-}
make9 :: [Bit] -> [Bit]
make9 bits = make8 bits ++ [parityBit bits]

-- Encode a string of characters as a list of bits
{-
encode "abc" -- [1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,1,1,1,0,0,0,1,1,0,0]
-}
encode' :: String -> [Bit]
encode' = concatMap $ make9 . int2bin . ord

-- Chop a list of bits into a list of groups of 9 bits
{-
chop9 (encode' "abc") -- [[1,0,0,0,0,1,1,0,1],[0,1,0,0,0,1,1,0,1],[1,1,0,0,0,1,1,0,0]]
-}
chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

-- Return the `init` of `bits` if the last, parity bit is correct - otherwise an error
{-
testParity [1,0,0,0,0,1,1,0,1] -- [1,0,0,0,0,1,1,0]
testParity [1,0,0,0,0,1,1,0,0] -- *** Exception: Parity error
-}
testParity :: [Bit] -> [Bit]
testParity bits | parityBit bs == last bits = bs
                | otherwise                 = errorWithoutStackTrace "Parity error"
  where bs = init bits

-- Decode a list of bits into a string, checking the parity bits, and returning an error
-- message if any parity bit is wrong
{-
decode' [1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,1,1,1,0,0,0,1,1,0,0] -- "abc"
decode' [0,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,1,1,1,0,0,0,1,1,0,0] -- *** Exception: Parity error
-}
decode' :: [Bit] -> String
decode' = map (chr . bin2int . testParity) . chop9

-- A faulty communication channel that forgets the first bit
channel' :: [Bit] -> [Bit]
channel' = tail

-- Simulate the transmission of a string of characters as a list of bits on a faulty channel
{-
transmit' "abc" -- *** Exception: Parity error
-}
transmit' :: String -> String
transmit' = decode' . channel' . encode'
