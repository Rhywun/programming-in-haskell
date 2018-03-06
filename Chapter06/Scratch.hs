module Chapter06.Scratch where

-- 6.1 - Basic concepts

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 6.2 - Recursion on lists

product' :: [Int] -> Int
product' [] = 1
product' (n:ns) = n * product' ns

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

-- Insertion sort
isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- 6.3 - Multiple arguments                                   <-- Continue here

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n - 1) xs

-- 6.4 - Multiple recursion

-- Return the first n items of the Fibonacci sequence - very inefficient!
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                   smaller = [a | a <- xs, a <= x]
                   larger  = [b | b <- xs, b > x]

-- 6.5 - Mutual recursion

even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n - 1)

-- Select the elements from a list at all even positions
evens :: [a] -> [a]
evens []= []
evens (x:xs) = x: odds xs

-- Select the elements from a list at all odd positions
odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

-- 6.6 - Advice on recursion
-- See text for full discussion of `product`, `drop`, and `init`

-- Removes the last element from a non-empty list
init' :: [a] -> [a]
init' [_]    = []
init' (x:xs) = x : init' xs
