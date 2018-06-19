{-# LANGUAGE InstanceSigs #-}

module Chapter14.Scratch where

import           Data.Monoid
import           Data.Foldable

--
-- 14.1 - Monoids
--

{-
class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a

  mconcat :: [a] -> a
  mconcat = foldr mappend mempty

Laws:
mempty `mappend` x == x
x `mappend` mempty == x
x `mappend` (y `mappend` z) == (x `mappend` y) `mappend` z
-}

-- Examples

{-
instance Monoid [a] where
  mempty = []
  mappend = (++)
-}

{-
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` my = my
  mx `mappend` Nothing = mx
  Just x `mappend` Just y = Just (x `mappend` y)
-}

{-
Sum, Product, All, Any: see text
-}

m1 :: Product Int
m1 = mconcat [Product 2, Product 3] -- Product {getProduct = 6}

m2 :: All
m2 = mconcat [All True, All True, All True] -- All {getAll = True}

--
-- 14.2 - Foldables
--

foldList :: Monoid a => [a] -> a
foldList []       = mempty
foldList (x : xs) = x `mappend` foldList xs

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

foldTree :: Monoid a => Tree a -> a
foldTree (Leaf x  ) = x
foldTree (Node l r) = foldTree l `mappend` foldTree r

{-
class Foldable t where
  fold    :: Monoid a => t a -> a
  foldMap :: Monoid b => (a -> b) -> t a -> b
  foldr   :: (a -> b -> b) -> b -> t a -> b
  foldl   :: (a -> b -> a) -> a -> t b -> a
-}

-- Examples

-- List - see text for instance definitions

f1 :: Sum Integer
f1 = foldMap Sum [1 .. 10] -- Sum {getSum = 55}

f2 :: Product Integer
f2 = foldMap Product [1 .. 10] -- Product {getProduct = 3628800}

-- Tree

instance Foldable Tree where
  fold :: Monoid a => Tree a -> a
  fold (Leaf x)   = x
  fold (Node l r) = fold l `mappend` fold r

  foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x)   = f x
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f v (Leaf x)   = f x v
  foldr f v (Node l r) = foldr f (foldr f v r) l

  foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f v (Leaf x)   = f v x
  foldl f v (Node l r) = foldl f (foldl f v l) r

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

f3 :: Int
f3 = foldr (+) 0 tree -- 1 + (2 + (3 + 0)) = 6

f4 :: Int
f4 = foldl (+) 0 tree -- ((0 + 1) + 2) + 3 = 6

-- Other primitives and defaults

{-
null    :: t a -> Bool
length  :: t a -> Int
elem    :: Eq a => a -> t a -> Bool
maximum :: Ord a => t a -> a
minimum :: Ord a => t a -> a
sum     :: Num a => t a -> a
product :: Num a => t a -> a

  null []                             -- True
  null (Leaf 1)                       -- False
  length [1..10]                      -- 10
  length (Node (Leaf 'a') (Leaf 'b')) -- 2

foldr1 :: (a -> a -> a) -> t a -> a
foldl1 :: (a -> a -> a) -> t a -> a

  foldr1 (+) [1..10]                  -- 55
  foldl1 (+) (Node (Leaf 1) (Leaf 2)) -- 3

toList :: t a -> [a]

  toList (Node (Leaf 1) (Leaf 2)) -- [1,2]

See text for how `toList` can be used to define the other primitives in terms
of the corresponding primitives for lists.
-}
