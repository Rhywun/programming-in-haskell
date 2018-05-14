module Chapter05.Caesar where

import           Data.Char

--
-- Encoding and decoding
--
-- Converts a char 'a' to 'z' to an integer 0 to 25
{-
char2int 'h' -- 7
-}
char2int :: Char -> Int
char2int c = ord c - ord 'a'

-- Converts an integer 0 to 25 to a char 'a' to 'z'
{-
int2char 7 -- 'h'
-}
int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

-- Shifts char c to the right by n letters; operates only on lower-case letters;
-- wraps at alphabet end
{-
char2int 3 'h' -- 'k'
-}
shift :: Int -> Char -> Char
shift n c
  | isAsciiLower c = int2char ((char2int c + n) `mod` 26)
  | otherwise = c

-- Encodes string xs with shift factor n
-- E.g. encode 3 "haskell is fun" = "kdvnhoo lv ixq"
-- E.g. encode -3 "kdvnhoo lv ixq" = "haskell is fun"
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

--
-- Frequency tables
--
-- Percentage frequencies for the 26 letters of the English alphabet based on a
-- large sample of text
table :: [Float]
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

-- n is what percent of m
-- E.g. percent 5 15 = 33.333336
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- Counts the number of lower-case chars in string xs
-- E.g. lowers "Hello" = 4
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- Counts the number of occurrences of x in xs
-- E.g. count 'l' "hello" = 2
count x xs = length [x' | x' <- xs, x == x']

-- Returns a frequency table based on just the string xs
-- E.g. freqs "abbcccddddeeeee" = [6.666667, 13.333334, 20.0, 26.666668,..., 0.0]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where
    n = lowers xs

--
-- Cracking the cipher
--
-- Chi-square statistic to compare a list of observed frequencies `os` with a list of
-- expected frequencies `es` - the smaller the result, the better match between the lists
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

-- Rotates the elements of list xs n places to the left, wrapping around at the start
-- of the list
-- E.g. rotate 3 [1,2,3,4,5] = [4,5,1,2,3]
rotate n xs = drop n xs ++ take n xs

-- Returns the indexes of all occurrences of x in xs
-- E.g. positions 'l' "hello" = [2, 3]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- Cracks the code
-- E.g. crack $ encode 3 "haskell is fun" = "haskell is fun"
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs-- Unfortunately it doesn't always work
-- E.g. crack (encode 3 "boxing wizards jump quickly") = "wjsdib rduvmyn ephk lpdxfgt"
