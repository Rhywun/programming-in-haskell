module Chapter03.Scratch where

--
-- 3.5 - Function types
--
add :: (Int, Int) -> Int
add (x, y) = x + y

zeroTo :: Int -> [Int]
zeroTo n = [0 .. n]

-- There is no requirement that a function must return something for every value:
ft1 = head [] -- *** Exception: Prelude.head: empty list

--
-- 3.6 - Curried functions
--
add' :: Int -> (Int -> Int) -- i.e. Int -> Int -> Int
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int)) -- i.e. Int -> Int -> Int -> Int
mult x y z = x * y * z

--
-- 3.7 - Polymorphic types
--
-- length :: [a] -> Int
pt1 = length [sin, cos, tan] -- 3

-- fst  :: (a, b) -> a
pt2 = fst (2, 3)

--
-- 3.8 - Overloaded types
--
-- (+) :: Num a => a -> a -> a
-- 3 :: Num a => a

