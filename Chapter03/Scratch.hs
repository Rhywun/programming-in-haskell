-- 3.5

add :: (Int, Int) -> Int
add (x, y) = x + y

zeroTo :: Int -> [Int]
zeroTo n = [0..n]

-- exception: undefined
s0305 = head[]

-- 3.6

add' :: Int -> (Int -> Int)
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

-- 3.7

s0307 = length [sin, cos, tan]