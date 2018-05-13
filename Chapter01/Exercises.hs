module Chapter01.Exercises where

--
-- 1
{-
double (double 2)
double (2 + 2)
(2 + 2) + (2 + 2)
4 + 4
8
-}
--
-- 2
{-
sum [x]
x + sum []
x + 0
x
-}
--
-- 3
{-
product' [2,3,4] -- 24
-}
product' []     = 0
product' (x:xs) = x * product xs

-- 4
-- The original:
{-
qsort [3,5,1,4,2] -- [1,2,3,4,5]
-}
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- Reverse sort:
{-
qsort' [3,5,1,4,2] -- [5,4,3,2,1]
-}
qsort' [] = []
qsort' (x:xs) = qsort' larger ++ [x] ++ qsort' smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- 5
-- Duplicates are discarded:
{-
qsort'' [2,2,3,1,1] -- [1,2,3]
-}
qsort'' [] = []
qsort'' (x:xs) = qsort'' smaller ++ [x] ++ qsort'' larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]
