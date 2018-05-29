module Chapter08.Exercises where

-- A natural number
data Nat = Zero | Succ Nat deriving (Eq, Show)

n2 = Succ (Succ Zero)
n3 = Succ (Succ (Succ Zero))

-- Add two natural numbers
{-
add n2 n3 -- Succ (Succ (Succ (Succ (Succ Zero))))
-}
add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

-- 1

-- Multiply two natural numbers
{-
mult n2 n3 -- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
-}
mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult m n    = add m (mult m (prec n))
 where
  prec (Succ x) = x
  prec Zero     = undefined

-- 2

-- A tree
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

tree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- Replace this with a better version using `compare`:
{-
occurs' 2 tree -- False
-}
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) | x == y    = True
                       | x < y     = occurs' x l
                       | otherwise = occurs' x r

-- Return whether a given value occurs in the tree
{-
occurs 2 tree -- False
-}
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y    ) = x == y
occurs x (Node l y r) = case compare x y of
  LT -> occurs x l
  EQ -> True
  GT -> occurs x r

{-
I am skeptical of the author's claim of `compare` being more efficient due to involving fewer
comparisons of x and y. Under the covers, `compare` is doing the same comparisons as the
original code! So wouldn't the extra function call in fact make this version LESS efficient?
-}

-- 3

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving (Eq, Show)

-- A balanced tree
tree' = Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Leaf' 4))

-- An unbalanced tree
tree'' = Node'
  (Node' (Leaf' 1) (Leaf' 2))
  (Node' (Node' (Node' (Leaf' 3) (Leaf' 4)) (Node' (Leaf' 5) (Leaf' 6)))
         (Leaf' 7)
  )

-- Count the number of leaves in a tree
{-
leaves tree'  -- 4
leaves tree'' -- 7
-}
leaves :: Tree' a -> Int
leaves (Leaf' _  ) = 1
leaves (Node' l r) = leaves l + leaves r

-- Return whether a tree is balanced - I cheated here but I get it
{-
balanced tree'  -- True
balanced tree'' -- False
-}
balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) =
  abs (leaves l - leaves r) <= 1 && balanced l && balanced r

-- 4
-- Cheat

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

{-
balance [1,2,3,4,5,6,7,8,9]
-}
balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs  = Node' (balance ys) (balance zs) where (ys, zs) = halve xs

-- 5

-- An expression consists of values and the Add operator
data Expr = Val Int | Add Expr Expr deriving (Eq, Show)

expr1 = Add (Val 1) (Val 2)
expr2 = Add (Val 1) (Add (Val 2) (Val 3))

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n  ) = f n
folde f g (Add l r) = g (folde f g l) (folde f g r)

-- 6

-- Evaluate an expression
-- E.g. eval e == 3
-- E.g. eval e' == 6
eval :: Expr -> Int
eval = folde id (+)

-- Count the number of values in an expression
-- E.g. size e == 2
-- E.g. size e' == 3
size :: Expr -> Int
size = folde (const 1) (+)

-- 7

{-
instance Eq a => Eq (Maybe a) where
  Nothing == _     = Nothing
  _ == Nothing     = Nothing
  Just x == Just y = x == y

instance Eq a => Eq [a] where
  [] == [] = True
  (x:xs) == (y:ys) = x == y && xs == ys
-}

-- 8
-- See Tautology.hs
-- PASS on the "logical equivalence (<=>)" bit, because WTF?

-- 9
-- See Machine.hs
