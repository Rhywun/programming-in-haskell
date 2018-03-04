-- 4.3 - Guarded equations can be easier to read than if/then/else

-- abs' :: (Ord t, Num t) => t -> t
abs' n | n >= 0    = n
       | otherwise = -n

-- signum' :: (Ord a, Num a, Num t) => a -> t
signum' n | n < 0     = -1
          | n == 0    = 0
          | otherwise = 1

-- 4.4 - Pattern matching

not' :: Bool -> Bool
not' False = True
not' True  = False

test :: [Char] -> Bool
test ('a':_) = True
test _       = False

-- 4.5 - Lambda expressions

double = (\x -> x + x)

-- `f` doesn't need a name...
odds n = map f [0..n - 1] where f x = x * 2 + 1

-- ...so get rid of it
odds' n = map (\x -> x * 2 + 1) [0..n - 1]

-- 4.6 - Operator sections

sum' :: [Int] -> Int
sum' = foldl (+) 0
