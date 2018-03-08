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
map'' f = unfold null f f                                     -- <-- Continue here

{-
-- iterate: skip

-- 7: skip

-- 8: skip

-- 9
{-
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 (x:xs) = cycle
-}
-- No idea, giving up
-- I want to use `cycle` over [1,2] to determine which function to apply to xs
-}