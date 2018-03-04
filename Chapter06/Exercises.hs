-- 1

factorial :: Int -> Int
factorial 0 = 1
factorial n | n > 0     = n * factorial (n - 1)
            | otherwise = error "Can't take factorial of negative number"

-- 2

-- E.g. sumdown 3 = 3 + 2 + 1 + 0 = 6
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3

-- E.g. 2 `pow` 3 = 8
pow :: Int -> Int -> Int
x `pow` 0 = 1
x `pow` y = x * (x `pow` (y - 1))

-- 4

-- E.g. euclid 6 27
--      euclid 6 21
--      euclid 6 15
--      euclid 6 9
--      euclid 6 3
--      euclid 3 3
--      3
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x < y  = euclid x (y - x)
           | x > y  = euclid (x - y) y

-- 5

{-
length [1, 2, 3]
1 + length [2, 3]
1 + 1 + length [2]
1 + 1 + 1 + length []
1 + 1 + 1 + 0
3

drop 3 [1, 2, 3, 4, 5]
drop 2 [2, 3, 4, 5]
drop 1 [3, 4, 5]
drop 0 [4, 5]
[4, 5]

init [1, 2, 3]
1 : init [2, 3]
1 : 2 : init [3]
1 : 2 : []
[1, 2]
-}

-- 6

and' :: [Bool] -> Bool
and' [True] = True
-- and' [False] = False
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [xs] = xs
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

-- E.g. "hello" `nth` 1 = 'e'
nth :: [a] -> Int -> a
nth (x:xs) 0 = x
nth (x:xs) n = nth xs (n - 1)

-- E.g. elem' 'o' "brown" = True
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) | x == e    = True
               | otherwise = elem' e xs

-- 7

-- E.g. merge [2,5,6] [1,3,4] = [1,2,3,4,5,6]
-- TODO: This doesn't work
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 8

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort lefts) (msort rights)
    where (lefts, rights) = halve xs

-- 9

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' _ []    = []
take' 0 _     = []
take' n (x:xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last xs
