{-# LANGUAGE InstanceSigs #-}

module Chapter12.Exercises where

-- 1

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

t1 = Node Leaf 2 Leaf
t2 = Node (Node Leaf 3 Leaf) 4 Leaf
t3 = Node (Node Leaf 5 (Node Leaf 6 Leaf)) 7 (Node (Node Leaf 8 Leaf) 9 Leaf)

{-
fmap (+2) t1          -- Node Leaf 4 Leaf
fmap (+2) t2          -- Node (Node Leaf 5 Leaf) 6 Leaf
fmap (\x -> x - 2) t3 -- Node (Node Leaf 3 (Node Leaf 4 Leaf))
                            5 (Node (Node Leaf 6 Leaf) 7 Leaf)
-}
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf         = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- 2

{-
instance Functor ((->) a) where
  fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)
-}

-- 3

{-
instance Applicative ((->) a) where
  pure :: b -> (a -> b)
  pure = const

  (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  g <*> h = \x -> g x (h x)
-}

-- 4

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  pure :: a -> ZipList a
  pure x = Z (repeat x)

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

-- 5
-- ?

-- 6
{-
instance Monad ((->) a) where
  (>>=) :: m a -> (a -> m b) -> m b
  f >>= k = \a -> k (f a) a
-}

-- 7
-- PASS

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

expr :: Expr Int
expr = Add (Val 2) (Val 3)

instance Functor Expr where
  fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var x) = Var (g x)
  fmap _ (Val x) = Val x
  fmap g (Add l r) = Add (fmap g l) (fmap g r)

{-
instance Applicative Expr where
  pure :: a -> Maybe a
-}

-- 8
-- PASS
