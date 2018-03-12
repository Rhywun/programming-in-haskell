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

{-
instance Applicative Maybe where
  pure :: a -> Maybe a
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

-- We can rewrite the following in applicative style:

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x * y | x <- xs, y <- ys]

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

-- Effectful programming

{-
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs
-}

getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)

-- Applicative laws
{-
  pure id <*> x   = x
  pure (g x)      = pure g <*> pure x
  x <*> pure y    = pure (\g -> g y) <*> x
  x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-}

{-
  pure g <*> x1 <*> x2 <*> ... <*> xn   <-- "applicative style"
  fmap g x = pure g <*> x               <-- fmap can be defined with the applicative primitives
  g <$> x = fmap g x                    <-- <$> is an infix variant of fmap
  g <$> x1 <*> x2 <*> ... <*> nx        <-- thus, an alternate applicative style using infix <$>
-}

--
-- 12.3 - Monads
--

-- Consider:

data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Div x y) = eval x `div` eval y

-- But:
-- eval (Div (Val 1) (Val 0)) --> *** Exception: divide by zero

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv n m = Just (n `div` m)

eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
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

eval'' :: Expr -> Maybe Int
eval'' (Val n)   = Just n
eval'' (Div x y) = eval'' x >>=
                     \n -> eval'' y >>=
                       \m -> safeDiv n m

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

eval''' :: Expr -> Maybe Int
eval''' (Val n)   = Just n
eval''' (Div x y) = do n <- eval''' x
                       m <- eval''' y
                       safeDiv n m

{-
class Applicative m => Monad m where
  return :: a -> m a
  return = pure

  (>>=) :: m a -> (a -> m b) -> m b
-}

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

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do { x <- xs; y <- ys; return (x, y) }





-- Cont. p. 168
