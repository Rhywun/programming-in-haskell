-- 7.1 - Basic concepts

-- Due to currying, this...
add :: Integer -> Integer -> Integer
add x y = x + y

-- ...is equivalent to this
add' :: Integer -> Integer -> Integer
add' = \x -> (\y -> x + y)

-- A function that takes a function as an argument
twice :: (a -> a) -> a -> a
twice f x = f (f x)

quadruple = twice (*2)

-- 7.2 - Processing lists

m1 = map (+1) [1, 3, 5, 7]
m2 = map even [1, 2, 3, 4]
m3 = map reverse ["abc", "def", "ghi"]
m4 = map (map (+1)) [[1, 2, 3], [4, 5]]

f1 = filter even [1..10]
f2 = filter (> 5) [1..10]

-- Returns the sum of the squares of even integers from list ns
sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

-- 7.3 - The foldr function

sum' :: Num a => [a] -> a           -- Note that the type annotation is required here...
sum' = foldr (+) 0

sum'' xs = foldr (+) 0 xs           -- ...but not here

length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0

-- "Reverse cons"
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' = foldr snoc []

-- 7.4 - The foldl function

length'' :: [a] -> Int
length'' = foldl (\n _ -> n + 1) 0

reverse'' :: [a] -> [a]
reverse'' = foldl (\xs x -> x:xs) []

-- 7.5 - The composition operator

-- We can rewrite these more simply:

twice' :: (a -> a) -> a -> a
-- twice f x = f (f x)
twice' f     = f . f

sumsqreven' :: [Int] -> Int
-- sumsqreven ns = sum (map (^2) (filter even ns))
sumsqreven'      = sum . map (^2) . filter even

-- Wut?
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- 7.6
-- See BinaryStringTransmitter.hs
