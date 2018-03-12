{-# LANGUAGE InstanceSigs #-}

module Chapter12.Scratch where

--
-- 12.1 - Functors
--

inc :: [Int] -> [Int]
inc []     = []
inc (n:ns) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr []     = []
sqr (n:ns) = n ^ 2 : sqr ns

-- We can abstract out a pattern:

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- Which allows:

inc' = map' (+1)
sqr' = map' (^2)

--

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)   = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- Generalisation:

inc'' :: Functor f => f Int -> f Int
inc'' = fmap (+1)

-- Functor laws:

{-
fmap id = id
fmap (g . h) = fmap g. fmap h
-}

--
-- 12.2 - Applicatives
--

-- cont. p. 160
