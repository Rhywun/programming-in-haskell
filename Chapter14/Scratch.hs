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

Finally:

fold      = foldMap id
foldMap f = foldr (mappend . f) mempty
toList    = foldMap (\x -> [x])
-}

-- Generic functions

average' :: [Int] -> Int
average' ns = sum ns `div` length ns

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

gf1 :: Int
gf1 = average [1 .. 10] -- 5

gf2 :: Int
gf2 = average (Node (Leaf 1) (Leaf 3)) -- 2

gf3 :: Bool
gf3 = and [True, False, True] -- False

gf4 :: Bool
gf4 = or (Node (Leaf True) (Leaf False)) -- True

gf5 :: Bool
gf5 = all even [1, 2, 3] -- False

gf6 :: Bool
gf6 = any even (Node (Leaf 1) (Leaf 2)) -- True

gf7 :: [Char]
gf7 = concat ["ab", "cd", "ef"] -- "abcdef"

gf8 :: [Int]
gf8 = concat (Node (Leaf [1, 2]) (Leaf [3])) -- [1,2,3]

--
-- 14.3 - Traversables
--

-- Compare:

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' g (x : xs) = g x : map g xs

-- Like `map` except the f can fail
traverse' :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse' _ []       = pure []
traverse' g (x : xs) = (:) <$> g x <*> traverse' g xs

-- Example:

-- Decrements an integer if it is positive
decIfPos :: Int -> Maybe Int
decIfPos n = if n > 0 then Just (n - 1) else Nothing

t1 :: Maybe [Int]
t1 = traverse' decIfPos [1, 2, 3] -- Just [0,1,2]

t2 :: Maybe [Int]
t2 = traverse' decIfPos [2, 1, 0] -- Nothing

{-
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a > f (t b)
-}

-- Examples:

-- List

{-
instance Traversable [] where
  traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
  traverse _ [] = pure []
  traverse g (x : xs) = (:) <$> g x <*> traverse g xs
-}

-- Tree

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)   = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g (Leaf x) = pure Leaf <*> g x
  traverse g (Node l r) = Node <$> traverse g l <*> traverse g r

t3 :: Maybe (Tree Int)
t3 = traverse decIfPos (Node (Leaf 1) (Leaf 2)) -- Just (Node (Leaf 0) (Leaf 1))

t4 :: Maybe (Tree Int)
t4 = traverse decIfPos (Node (Leaf 0) (Leaf 1)) -- Nothing

-- Other primitives and defaults

-- Transforms a data structure whose elements may fail (for example) into
-- a data structure that may fail
{-
sequenceA :: Applicative f => t (f a) -> f (t a)
sequenceA = traverse id
-}

t5 :: Maybe [Integer]
t5 = sequenceA [Just 1, Just 2, Just 3] -- Just [1,2,3]

t6 :: Maybe [Integer]
t6 = sequenceA [Just 1,Nothing,Just 3] -- Nothing

t7 :: Maybe (Tree Integer)
t7 = sequenceA (Node (Leaf (Just 1)) (Leaf (Just 2))) -- Just (Node (Leaf 1) (Leaf 2))

t8 :: Maybe (Tree Integer)
t8 = sequenceA (Node (Leaf (Just 1)) (Leaf Nothing)) -- Nothing

{-
  traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
  traverse g = sequenceA . fmap g
-}

