module Chapter04.Exercises where

  -- 1

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
           where n = length xs `div` 2

-- 2a

third :: [a] -> a
third xs = head (tail (tail xs))

-- 2b

third' :: [a] -> a
third' xs = xs !! 2

-- 2c

third'' :: [a] -> a
third'' (_:_:x:_) = x

-- 3a

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- 3b

safetail' :: [a] -> [a]
safetail' xs | null xs   = []
             | otherwise = tail xs

-- 3c

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

-- 4

{-
(||) :: Bool -> Bool -> Bool
True  || True  = True
True  || False = True
False || True  = True
False || False = False

(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True

(||) :: Bool -> Bool -> Bool
False || b = b
True  || _ = True

(||) :: Bool -> Bool -> Bool
b || c | b == c    = b
       | otherwise = True
-}

-- 5

myAnd x y =
    if x
        then
            if y
                then True
                else False
        else False

-- 6

myAnd' x y =
    if x
        then y
        else False

-- 7

mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

mult' :: Int -> Int -> Int -> Int
mult' = \x -> (\y -> (\z -> x * y * z))

-- 8

-- Double a digit and subtract 9 if the result is greater than 9
luhnDouble :: Int -> Int
luhnDouble x = if d > 9 then d - 9 else d
               where d = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn i j k l = n `mod` 10 == 0
               where n = luhnDouble i + j + luhnDouble k + l
