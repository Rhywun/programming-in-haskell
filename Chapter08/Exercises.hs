module Chapter08.Exercises where

-- A natural number
data Nat = Zero | Succ Nat deriving (Eq, Show)

n2 = Succ (Succ Zero)
n3 = Succ (Succ (Succ Zero))

-- Add two natural numbers
-- E.g. add n2 n3 == Succ (Succ (Succ (Succ (Succ Zero))))
add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

-- 1

-- Multiply two natural numbers
-- E.g. mult n2 n3 == Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult m n    = add m (mult m (pred' n))
              where pred' (Succ x) = x

-- 2

-- A tree
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

-- Replace this with a better version using `compare`:
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)     = x == y
occurs' x (Node l y r) | x == y    = True
                       | x < y     = occurs' x l
                       | otherwise = occurs' x r

-- Return whether a given value occurs in the tree
-- E.g. occurs 2 t == False
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
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
t' :: Tree' Int
t' = Node' (Node' (Leaf' 1) (Leaf' 2))
           (Node' (Leaf' 3) (Leaf' 4))

-- An unbalanced tree
t'' :: Tree' Int
t'' = Node' (Node' (Leaf' 1) (Leaf' 2))
            (Node' (Node' (Node' (Leaf' 3) (Leaf' 4)) (Node' (Leaf' 5) (Leaf' 6))) (Leaf' 7))

-- Count the number of leaves in a tree
leaves :: Tree' a -> Int
leaves (Leaf' _)   = 1
leaves (Node' l r) = leaves l + leaves r

-- Return whether a tree is balanced - I cheated here but I get it
balanced :: Tree' a -> Bool
balanced (Leaf' _)   = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

-- 4
-- Cheat

halve xs = splitAt (length xs `div` 2) xs

-- E.g. balance [1,2,3,4,5,6,7,8,9]
balance [x] = Leaf' x
balance xs  = Node' (balance ys) (balance zs)
              where (ys, zs) = halve xs

-- 5

data Expr = Val Int | Add Expr Expr deriving (Eq, Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
-- lolwut
