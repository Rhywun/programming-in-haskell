module Chapter07.Scratch where

--
-- 7.1 - Basic concepts
--
-- Due to currying, this...
add :: Integer -> Integer -> Integer
add x y = x + y

-- ...is equivalent to this
add' :: Integer -> Integer -> Integer
add' = \x -> (\y -> x + y)

-- A function that takes a function as an argument:
{-
twice (* 2) 3 -- 12
twice reverse "hello" -- "hello"
-}
twice :: (a -> a) -> a -> a
twice f x = f (f x)

{-
quadruple 4 -- 16
-}
quadruple = twice (* 2)

--
-- 7.2 - Processing lists
--
m1 = map (+ 1) [1, 3, 5, 7] -- [2,4,6,8]

m2 = map even [1, 2, 3, 4] -- [False,True,False,True]

m3 = map reverse ["abc", "def", "ghi"] -- ["cba","fed","ihg"]

m4 = map (map (+ 1)) [[1, 2, 3], [4, 5]] -- ["cba","fed","ihg"]

f1 = filter even [1 .. 10] -- [2,4,6,8,10]

f2 = filter (> 5) [1 .. 10] -- [6,7,8,9,10]

f3 = filter (/= ' ') "abc def ghi" -- "abcdefghi"

-- Combining `map` and `filter`
--
-- Returns the sum of the squares of even integers from list ns
{-
sumsqreven [1..10]
--> 4 + 16 + 36 + 64 + 100
--> 220
-}
sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^ 2) (filter even ns))

--
-- 7.3 - The foldr function
--
sum' :: Num a => [a] -> a -- Note that the type annotation is required here...
sum' = foldr (+) 0

sum'' xs = foldr (+) 0 xs -- ...but not here

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)

length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0

-- "Reverse cons"
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' = foldr snoc []

--
-- 7.4 - The foldl function
--
sum''' xs = foldl (+) 0 xs -- (+) is associative

length'' :: [a] -> Int
length'' = foldl (\n _ -> n + 1) 0

reverse'' :: [a] -> [a]
-- reverse'' = foldl (\xs x -> x:xs) []
reverse'' = foldl (flip (:)) [] -- Recommended by hlint

--
-- 7.5 - The composition operator
--
-- This definition makes it clear that a function is returned:
{-
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
-}
--
-- We can now rewrite some function definitions more simply:
--
-- odd n = not (even n)
odd' = not . even

-- twice f x = f (f x)
twice' f = f . f

-- sumsqreven ns = sum (map (^2) (filter even ns))
sumsqreven' = sum . map (^ 2) . filter even

-- The composition of a list of functions uses `id` as the identity (or "starting point"):
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id
--
--
-- 7.6 - Binary string transmitter
-- See Binary.hs
--
--
-- 7.7 - Voting algorithms
-- See Voting.hs
