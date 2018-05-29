module Chapter02.Exercises where

-- 2
--
e2a = (2 ^ 3) * 4 == 2 ^ 3 * 4

e2b = (2 * 3) + (4 * 5) == 2 * 3 + 4 * 5

e2c = 2 + (3 * (4 ^ 5)) == 2 + 3 * 4 ^ 5

-- 3
{-
n -- 2
-}
n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

-- 4
--
{-
last' [1..5] -- 5
-}
last' xs = xs !! (length xs - 1)

{-
last'' [1..5] -- 5
-}
last'' xs = head (reverse xs)

-- 5
--
{-
init' [1..5] -- [1,2,3,4]
-}
init' xs = take (length xs - 1) xs

{-
init'' [1..5] -- [1,2,3,4]
-}
init'' xs = reverse (tail (reverse xs))
