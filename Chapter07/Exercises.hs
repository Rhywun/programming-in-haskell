module Chapter07.Exercises where

-- 1

{-
    [f x | x <- xs, p x]
    map f (filter p xs)
-}

-- 2

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                 = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []                 = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x : xs
-- 3

map' :: (a -> b) -> [a] -> [b]
-- map f = foldr (\x -> f x) []
map' f = foldr (\x xs -> f x : xs) []

filter':: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- 4

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> y + 10 * x) 0

-- 5
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

-- 6

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
-- int2bin 0 = []
-- int2bin n = n `mod` 2 : int2bin (n `div` 2)
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
-- chop8 []   = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)
chop8 = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
-- map f []     = []
-- map f (x:xs) = f x : map f xs
map'' f = unfold null (f . head) tail         -- applies `f` to the head of the list with each
                                              -- recusion until the list is exhausted

iterate' :: Eq a => (a -> a) -> a -> [a]
-- iterate f x =  x : iterate f (f x)
iterate' = unfold (\x -> x /= x) id           -- the `p` expression is always false, therefore
                                              -- execution never ends
                                              -- the `h` expression is `id` and therefore has
                                              -- no effect on the `x` value
-- 7: skip
-- See Binary2.hs

-- 8
-- See Binary2.hs

-- 9

-- Alternately apply one of two functions to successive elements in a list
-- E.g. altMap (+10) (+100) [0,1,2,3,4] == [10,101,12,103,14]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 (x:xs) = go f1 f2 (x:xs) 0
  where go _ _ [] _                       = []
        go f1' f2' (x':xs') n | even n    = f1' x' : go f1' f2' xs' (n + 1)
                              | otherwise = f2' x' : go f1' f2' xs' (n + 1)

-- 10

-- Double a digit and subtract 9 if the result is greater than 9
luhnDouble :: Int -> Int
luhnDouble x = if d > 9 then d - 9 else d
               where d = x * 2

{-
-- From ch04:
luhn :: Int -> Int -> Int -> Int -> Bool
luhn i j k l = n `mod` 10 == 0
               where n = luhnDouble i + j + luhnDouble k + l
-}

-- E.g. luhn [4,2,6,6,8,4,1,1,5,9,3,4,9,9,5,8] == True
-- E.g. luhn [5,2,5,1,0,8,0,1,2,9,0,1,1,3,7,2] == True
-- E.g. luhn [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] == False
-- E.g. luhn [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1] == False
luhn :: [Int] -> Bool
luhn xs = sum (altMap f1 f2 xs) `mod` 10 == 0
          where f1 | even $ length xs = luhnDouble
                   | otherwise        = id
                f2 | even $ length xs = id
                   | otherwise        = luhnDouble

-- Choose the functions f1 and f2 based on the length of the list:
--    even? f1 = luhnDouble, f2 = id
--    odd?  f1 = id, f2 = luhnDouble
