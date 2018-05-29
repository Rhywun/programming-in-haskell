module Chapter06.Scratch where

--
-- 6.1 - Basic concepts
--
{-
factorial 3 -- 6
-}
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

--
-- 6.2 - Recursion on lists
--
{-
product' [2,3,4] -- 24
-}
product' :: [Int] -> Int
product' []     = 1
product' (n:ns) = n * product' ns

{-
length' "hello" -- 5
-}
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

{-
reverse' [1,2,3] -- [3,2,1]
-}
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

{-
insert 3 [1,2,4,5] -- [1,2,3,4,5]
-}
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

-- Insertion sort
{-
isort [3,5,2,1,4] -- [1,2,3,4,5]
-}
isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

--
-- 6.3 - Multiple arguments
--
{-
zip ['a','b','c'] [1,2,3,4] -- [('a',1),('b',2),('c',3)]
-}
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

{-
drop' 2 [1,2,3,4,5] -- [3,4,5]
-}
drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n - 1) xs

--
-- 6.4 - Multiple recursion
--
-- Return the first n items of the Fibonacci sequence
-- - very inefficient already at n == 30!
{-
fib 29 -- 514229
-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

{-
qsort [3,5,2,1,4] -- [1,2,3,4,5]
-}
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

--
-- 6.5 - Mutual recursion
--
even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n - 1)

-- Select the elements from a list at all even positions
{-
evens "abcde" -- "ace"
-}
evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x : odds xs

-- Select the elements from a list at all odd positions
odds :: [a] -> [a]
odds []     = []
odds (_:xs) = evens xs

--
-- 6.6 - Advice on recursion
--
-- See text for full discussion of `product`, `drop`, and `init`
{-
Step 1: define the type
Step 2: enumerate the cases
Step 3: define the simple cases
Step 4: define the other cases
Step 5: generalise and simplify
-}
-- Removes the last element from a non-empty list
init' :: [a] -> [a]
init' [_]    = []
init' (x:xs) = x : init' xs
