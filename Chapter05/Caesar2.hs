module Chapter05.Caesar2 where

import           Data.Char

-- Encoding and decoding
--
char2int c
  | isLower c = ord c - ord 'a'
  | isUpper c = ord c - ord 'A'

int2charLower n = chr (ord 'a' + n)

int2charUpper n = chr (ord 'A' + n)

shift n c
  | isAsciiLower c = int2charLower ((char2int c + n) `mod` 26)
  | isAsciiUpper c = int2charUpper ((char2int c + n) `mod` 26)
  | otherwise = c

encode n xs = [shift n x | x <- xs]

-- Frequency tables
--
table =
  [ 8.1
  , 1.5
  , 2.8
  , 4.2
  , 12.7
  , 2.2
  , 2.0
  , 6.1
  , 7.0
  , 0.2
  , 0.8
  , 4.0
  , 2.4
  , 6.7
  , 7.5
  , 1.9
  , 0.1
  , 6.0
  , 6.3
  , 9.0
  , 2.8
  , 1.0
  , 2.4
  , 0.2
  , 2.0
  , 0.1
  ]

percent n m = (fromIntegral n / fromIntegral m) * 100

lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count x xs = length [x' | x' <- xs, x == x']

freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where
    n = lowers xs

-- Cracking the cipher
--
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate n xs = drop n xs ++ take n xs

positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

{-
crack $ encode 3 "Haskell is Fun" -- "Haskell is Fun"
-}
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs
