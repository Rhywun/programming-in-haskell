module Chapter03.Scratch where

-- 3.5

add :: (Int, Int) -> Int
add (x, y) = x + y

zeroTo :: Int -> [Int]
zeroTo n = [0..n]

-- exception: undefined
_ = head []

-- 3.6

add' :: Int -> (Int -> Int)           -- i.e. Int -> Int -> Int
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int))  -- i.e. Int -> Int -> Int -> Int
mult x y z = x * y * z

-- 3.7

_ = length [sin, cos, tan]            -- length is polymorphic
