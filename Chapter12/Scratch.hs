{-# LANGUAGE InstanceSigs #-}

module Chapter12.Scratch where

import           Data.Char

--
-- 12.1 - Functors
--

inc :: [Int] -> [Int]
inc []       = []
inc (n : ns) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr []       = []
sqr (n : ns) = n ^ 2 : sqr ns

-- We can abstract out a pattern:

map' :: (a -> b) -> [a] -> [b]
map' f []       = []
map' f (x : xs) = f x : map' f xs

-- Which allows:

inc' :: [Int] -> [Int]
inc' = map' (+ 1)

sqr' :: [Int] -> [Int]
sqr' = map' (^ 2)

--
-- Examples

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)   = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- Generalisation:

{-
inc'' (Just 1)                 -- Just 2
inc'' (Node (Leaf 1) (Leaf 2)) -- Node (Leaf 2) (Leaf 3)
-}
inc'' :: Functor f => f Int -> f Int
inc'' = fmap (+ 1)

--
-- Functor laws:

{-
  fmap id = id
  fmap (g . h) = fmap g . fmap h
-}

--
-- 12.2 - Applicatives
--

{-
instance Applicative Maybe where
  pure :: a -> Maybe a                <-- This is allowed with InstanceSigs
  pure = Just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _   = Nothing
  (Just g) <*> mx = fmap g mx
-}

{-
instance Applicative [] where
  pure :: a -> [a]
  pure x = [x]

  (<*>) :: [a -> b] -> [a] -> [b]
  gs <*> xs = [g x | g <- gs, x <- xs]
-}

--
-- Examples

-- We can rewrite the following in applicative style:

{-
prods [2,3] [4,5] --- [8,10,12,15]
-}
prods :: [Int] -> [Int] -> [Int]
prods xs ys = [ x * y | x <- xs, y <- ys ]

{-
prods' [2,3] [4,5] --- [8,10,12,15]
-}
prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = pure (*) <*> xs <*> ys

{-
instance Applicative IO where
  pure :: a -> IO a
  pure = return

  (<*>) :: IO (a -> b) -> IO a -> IO b
  mg <*> mx = do {g <- mg; x <- mx; return (g x)}
-}

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

--
-- Effectful programming

{-
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs
-}

-- Now we can redefine this more simply:

getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)

--
-- Applicative laws
{-
  pure id <*> x   = x
  pure (g x)      = pure g <*> pure x
  x <*> pure y    = pure (\g -> g y) <*> x
  x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-}

{-
  pure g <*> x1 <*> x2 <*> ... <*> xn <-- "applicative style"
  fmap g x = pure g <*> x             <-- fmap can be defined with applicative primitives
  g <$> x = fmap g x                  <-- <$> is an infix variant of fmap
  g <$> x1 <*> x2 <*> ... <*> nx      <-- thus, an alt. applicative style using infix <$>
-}

--
-- 12.3 - Monads
--

-- Consider:

data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n  ) = n
eval (Div x y) = eval x `div` eval y

-- But:
-- eval (Div (Val 1) (Val 0)) --> *** Exception: divide by zero

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv n m = Just (n `div` m)

{-
eval' (Div (Val 1) (Val 0)) -- Nothing
-}
eval' :: Expr -> Maybe Int
eval' (Val n  ) = Just n
eval' (Div x y) = case eval' x of
  Nothing -> Nothing
  Just n  -> case eval' y of
    Nothing -> Nothing
    Just m  -> safeDiv n m

-- Maybe we can use applicative style to simplify?

-- No - this is not type correct, because `safeDiv` is not a pure function:
{-
eval'' :: Expr -> Maybe Int
eval'' (Val n)   = pure n
eval'' (Div x y) = pure safeDiv <*> eval'' x <*> eval'' y
-}

-- Instead, we abstract out another pattern:
{-
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
           Nothing -> Nothing
           Just x  -> f x
-}

-- Which allows:

{-
eval'' (Div (Val 1) (Val 0)) -- Nothing
-}
eval'' :: Expr -> Maybe Int
eval'' (Val n  ) = Just n
eval'' (Div x y) = eval'' x >>= \n -> eval'' y >>= \m -> safeDiv n m

-- Monad style
{-
  m1 >>= \x1 -> m2 >>= \x2 -> ... mn >>= \xn -> f x1 x2 ... xn
-}

-- Or
{-
  do x1 <- m1
     x2 <- m2
     ...
     xn <- mn
     f x1 x2 ... xn
-}

{-
eval''' (Div (Val 1) (Val 0)) -- Nothing
-}
eval''' :: Expr -> Maybe Int
eval''' (Val n  ) = Just n
eval''' (Div x y) = do
  n <- eval''' x
  m <- eval''' y
  safeDiv n m

{-
class Applicative m => Monad m where
  return :: a -> m a
  return = pure

  (>>=) :: m a -> (a -> m b) -> m b
-}

--
-- Examples

{-
instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _  = Nothing
  (Just x) >>= f = f x
-}

{-
instance Monad [] where
  (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = [y | x <- xs, y <- f x]
-}

-- This has the effect of collecting all the results in a list:

{-
pairs [1,2] [3,4] -- [(1,3),(1,4),(2,3),(2,4)]
-}
pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

-- Q: How do we write this with (>>=)?

pairs' :: [a] -> [b] -> [(a, b)]
pairs' = undefined

{-
instance Monad IO where
  return :: a -> IO a
  return x = ...(built-in)...

  (>>=) :: IO a -> (a -> IO b) -> IO b
  mx >>= f = ...(built-in)...
-}

--
-- The state monad
-- See diagrams in the book!

type State = Int

-- A state transformer - including a return type `a`
newtype ST a = S (State -> (a, State))

-- Convenience function to remove the dummy constructor
app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  -- Applies a function to the result value of a ST
  fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative ST where
  -- Transforms a value into a ST that simply returns this value
  -- without modifying the state
  pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- Applies a ST that returns a function (`stf`) to a ST that returns an argument (`stx`)
  -- to give a ST that returns the result of applying the function to the argument (`f x`)
  (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s ->
                let (f, s')  = app stf s
                    (x, s'') = app stx s' in (f x, s''))

instance Monad ST where
  -- Applies a ST to an initial state `s`, then applies the function `f` to the
  -- resulting value `x` to give a new ST `f x`, which is then applied to the new
  -- state `s'` to give the final result
  (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

--
-- Relabelling trees
-- (an example of stateful programming)

-- Defined above:
-- data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- Relabel a tree with a sequence of ints, return the next fresh int:
-- (Notice that we need to thread an int state through the computation)
{-
fst $ rlabel tree 0 -- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)
snd $ rlabel tree 0 -- 3
-}
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _  ) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
 where
  (l', n' ) = rlabel l n
  (r', n'') = rlabel r n'

-- Let's use a ST instead, where the state is the next fresh integer

fresh = S (\n -> (n, n + 1)) :: ST Int

-- Rewrite `rlabel` in applicative style:
-- (remembering to apply `app` to the result in order to get at the S value)
{-
fst $ app (alabel tree) 0 -- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)
snd $ app (alabel tree) 0 -- 3
-}
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _  ) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- Monadic version:
-- (But kind of ugly if you ask me)
{-
fst $ app (mlabel tree) 0 -- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)
snd $ app (mlabel tree) 0 -- 3
-}
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do
  n <- fresh
  return (Leaf n)
mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (Node l' r')

--
-- Generic functions

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ []       = return []
mapM' f (x : xs) = do
  y  <- f x
  ys <- mapM' f xs
  return (y : ys)

-- Convert a digit char to an int
{-
mapM' conv "1234" -- Just [1,2,3,4]
mapM' conv "123a" -- Nothing
-- note:
map conv "1234"   -- [Just 1,Just 2,Just 3,Just 4]
-}
conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

{-
filterM' (\x -> [True,False]) [1,2,3] -- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
-}
-- "all possible ways of including (True) or excluding (False) each element of the list"
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ []       = return []
filterM' p (x : xs) = do
  b  <- p x
  ys <- filterM' p xs
  return (if b then x : ys else ys)

-- Generalisation of `concat` - flattens a nested monadic value
{-
join' [[1,2],[3,4],[5,6]] -- [1,2,3,4,5,6]
join' (Just (Just 1))     -- Just 1
join' (Just Nothing)      -- Nothing
join' Nothing             -- Nothing
-}
join' :: Monad m => m (m a) -> m a
-- join' mmx = do { mx <- mmx; x <- mx; return x }
join' mmx = do
  mx <- mmx
  mx

--
-- Monad laws

{-
  return x >>= f   = f x
  mx >>= return    = mx
  (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))
-}
