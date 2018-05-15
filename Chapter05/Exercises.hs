module Chapter05.Exercises where

-- 1
{-
e1 -- 338350
-}
e1 = sum [x ^ 2 | x <- [1 .. 100]]

-- 2
{-
grid 1 2 -- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
-}
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- 3
{-
square 2 -- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
-}
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4
{-
replicate' 3 True -- [True,True,True]
-}
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1 .. n]]

-- 5
{-
pyths 10 -- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-}
pyths :: Int -> [(Int, Int, Int)]
pyths n =
  [ (x, y, z)
  | x <- [1 .. n]
  , y <- [1 .. n]
  , z <- [1 .. n]
  , x ^ 2 + y ^ 2 == z ^ 2
  ]

-- 6
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

{-
perfects 500 -- [6,28,496]
-}
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], x == sum (factors x) - x]
  -- Try `perfects 10000` for some slow fun before it pops out 8128...

-- 7
--
e7 = [(x, y) | x <- [1, 2], y <- [3, 4]]

e7' = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]] -- I cheated
                                                    -- This hurts my brain

-- 8
--
-- Return list of all positions at which a value x occurs in a list xs
-- E.g. positions False [True, False, True, False] = [1,3]
-- E.g. positions 'l' "hello" = [2, 3]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- Return list of values associated with a given key k in table t
-- E.g. find 'b' [('a',1),('b',2),('c',3),('b',4)] = [2,4]
find k t = [v | (k', v) <- t, k == k']

positions' x xs = find x z
  where
    z = zip xs [0 .. (length xs - 1)]

-- 9
-- E.g. scalarproduct [1,2,3] [4,5,6] = 4 + 10 + 18 = 32
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
-- 10
-- See Caesar2.hs
