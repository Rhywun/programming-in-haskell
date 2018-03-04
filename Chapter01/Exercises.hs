-- 1

{-
double (double 2)
double (2 + 2)
(2 + 2) + (2 + 2)
4 + 4
8
-}

-- 2

{-
sum [x]
x + sum []
x + 0
x
-}

-- 3

product' []     = 0
product' (x:xs) = x * product xs

-- 4

-- The original:
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

-- Reverse sort:
qsort' [] = []
qsort' (x:xs) = qsort' larger ++ [x] ++ qsort' smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

-- 5

-- Duplicates are discarded:
qsort'' [] = []
qsort'' (x:xs) = qsort'' smaller ++ [x] ++ qsort'' larger
    where
        smaller = [a | a <- xs, a < x]
        larger  = [b | b <- xs, b > x]
