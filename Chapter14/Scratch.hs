module Chapter14.Scratch where

import Data.Monoid

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

{-
class Foldable t where
  fold    :: Monoid a => t a -> a
  foldMap :: Monoid b => (a -> b) -> t a -> b
  foldr   :: (a -> b -> b) -> b -> t a -> b
  foldl   :: (a -> b -> a) -> a -> t b -> a
-}

-- Examples

