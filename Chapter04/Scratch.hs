module Chapter04.Scratch where

--
-- 4.1 - New from old
--
{-
splitAt' 3 "hello" -- ("hel","lo")
-}
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

--
-- 4.2 - Conditional expressions
--
abs' :: Int -> Int
abs' n =
  if n > 0
    then n
    else -n

signum'' :: Int -> Int
signum'' n =
  if n < 0
    then -1
    else if n == 0
           then 0
           else 1

--
-- 4.3 - Guarded equations
--
abs'' :: Int -> Int
abs'' n
  | n >= 0 = n
  | otherwise = -n

-- Eliminates ugly nested if/then/else expressions:
--
signum'' :: Int -> Int
signum'' n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

--
-- 4.4 - Pattern matching
--
not' :: Bool -> Bool
not' False = True
not' True  = False

test :: String -> Bool
test ('a':_) = True
test _       = False

--
-- 4.5 - Lambda expressions
--
double = \x -> x + x

-- Note how the syntactic form of the definition reflects the type signature:
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

-- Highlight the fact that it returns a function:
const' :: a -> (b -> a)
const' x = \_ -> x

-- `f` doesn't need a name...
odds n = map f [0 .. n - 1]
  where
    f x = x * 2 + 1

-- ...so get rid of it
odds' n = map (\x -> x * 2 + 1) [0 .. n - 1]

--
-- 4.6 - Operator sections
--
sum' :: [Int] -> Int
sum' = foldl (+) 0
