module Chapter08.Scratch where

--
-- 8.1 - Type declarations
--

type Pos = (Int, Int)

type Trans = Pos -> Pos

-- Can't define a recursive type declaration with `type` - use `data` instead:
-- type Tree' = (Int, [Tree']) -- error: Cycle in type synonym declarations

type Pair a = (a, a)

type Assoc k v = [(k, v)]

assoc = [(1, "one"), (2, "two"), (3, "three")] :: Assoc Int String

-- Return the first value that is associated with a given key in a table
{-
find 2 assoc -- "two"
-}
find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k' ]

--
-- 8.2 - Data declarations
--

data Move = North | South | East | West deriving (Eq, Show)

-- Move in a direction
{-
move North (1, 1) -- (1, 2)
-}
move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East  (x, y) = (x + 1, y)
move West  (x, y) = (x - 1, y)

-- Apply a list of moves to a position
{-
moves [North, North, East] (1, 1) -- (2, 3)
-}
moves :: [Move] -> Pos -> Pos
moves ms p = foldl (flip move) p ms

-- Reverse a direction
{-
rev North -- South
-}
rev :: Move -> Move
rev North = South
rev South = North
rev East  = West
rev West  = East

--

data Shape = Circle Float | Rect Float Float deriving (Eq, Show)

-- Create a square of given size
{-
square 4 -- Rect 4.0 4.0
-}
square :: Float -> Shape
square n = Rect n n

-- Calculate area of a shape
{-
area (Circle 2) -- 12.566371
area (square 4) -- 16
-}
area :: Shape -> Float
area (Circle r) = pi * (r ^ 2 :: Float)
area (Rect x y) = x * y

--

{-
data Maybe a = Nothing | Just a
-}

-- A safe `div` which returns Nothing in case of invalid arguments
-- instead of an error
{-
4 `safediv` 0 -- Nothing
-}
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

-- A safe `head` which returns Nothing in case of an invalid argument
-- instead of an error
{-
safehead [] -- Nothing
-}
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

--
-- 8.3 - Newtype declarations
--

-- Natural number
newtype Nat' = N Int

nt1 = N 4

--
-- 8.4 - Recursive types
--

-- A natural number
data Nat = Zero | Succ Nat deriving (Eq, Show)

r04 = Zero
r11 = Succ Zero
r22 = Succ (Succ Zero)

-- Convert a natural number to an integer
{-
nat2int (Succ (Succ (Succ Zero))) -- 3
-}
nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

-- Convert an integer to a natural number
{-
int2nat 4 -- Succ (Succ (Succ (Succ Zero)))
-}
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

-- Add two natural numbers
{-
add' (Succ (Succ Zero)) (Succ Zero) -- (Succ (Succ (Succ Zero)))
-}
add' :: Nat -> Nat -> Nat
add' m n = int2nat (nat2int m + nat2int n)

-- Add two natural numbers (more efficiently)
{-
add (Succ (Succ Zero)) (Succ Zero) -- (Succ (Succ (Succ Zero)))
-}
add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

--

-- A list - must end in Nil!
data List' a = Nil | Cons a (List' a) deriving (Eq, Show)

list = Cons 1 (Cons 2 (Cons 3 Nil))

-- Return the length of a List'
{-
length' list -- 3
-}
length' :: List' a -> Int
length' Nil         = 0
length' (Cons _ xs) = 1 + length' xs

--

-- A tree
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

tree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- Return whether a given value occurs in the tree
{-
occurs' 2 tree -- False
-}
occurs' :: Eq a => a -> Tree a -> Bool
occurs' x (Leaf y    ) = x == y
occurs' x (Node l y r) = x == y || occurs' x l || occurs' x r

-- Flatten a tree to a list
{-
flatten tree -- [1,3,4,5,6,7,9]
-}
flatten :: Tree a -> [a]
flatten (Leaf x    ) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- IIf Tree is a search tree we can improve occurs' because this version
-- only traverses one path down the tree, rather than potentially the entire tree
{-
occurs 2 tree -- False
-}
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) | x == y    = True
                      | x < y     = occurs x l
                      | otherwise = occurs x r

--
-- 8.6 - Tautology checker
-- see Tautology.hs

--
-- 8.7 - Abstract machine
-- see Machine.hs
