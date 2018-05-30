module Chapter09.Countdown where

--
-- 9.2 - Arithmetic operators
--

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- Decide if the application of an operator to two positive naturals gives another
{-
valid Sub 2 3 -- False
valid Div 2 3 -- False
-}
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
-- valid Add x y = x <= y
valid Sub x y = x > y
valid Mul _ _ = True
-- valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

-- Perform an operation
{-
apply Add 1 2 -- 3
apply Div 8 2 -- 4
-}
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

--
-- 9.3 - Numeric expressions
--

data Expr = Val Int | App Op Expr Expr

{-
show expr -- "1+(2*3)"
-}
expr = App Add (Val 1) (App Mul (Val 2) (Val 3))

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where brak (Val n) = show n
                           brak e       = "(" ++ show e ++ ")"

-- Return the list of values in an expression
{-
values expr -- [1,2,3]
-}
values :: Expr -> [Int]
values (Val n    ) = [n]
values (App _ l r) = values l ++ values r

-- Evaluate an expression, disallowing non-natural values, and
-- returning an empty list on failure
{-
eval expr -- [7]                                 A valid expression
eval (App Sub (Val 2) (Val 3)) -- []          An invalid expression
-}
eval :: Expr -> [Int]
eval (Val n    ) = [ n | n > 0 ]
eval (App o l r) = [ apply o x y | x <- eval l, y <- eval r, valid o x y ]

--
-- 9.4 - Combinatorial functions
--

-- Return all subsequences of a list
{-
subs [1,2,3] -- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
-}
subs :: [a] -> [[a]]
subs []       = [[]]
subs (x : xs) = yss ++ map (x :) yss where yss = subs xs

-- Return all possible ways of inserting a new element into a list
{-
interleave 1 [2,3,4] -- [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
-}
interleave :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

-- Return all permutations of a list
{-
perms [1,2,3] -- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}
perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

-- Return all possible ways of selecting zero or more elements of a list in any order
-- I.e. map all permutations of all subsequences
{-
choices [1,2,3] -- [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],
                    [1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}
choices :: [a] -> [[a]]
choices = concatMap perms . subs
-- choices xs = [ zs | ys <- subs xs, zs <- perms ys ]                     -- Exercise 1

--
-- 9.5 - Formalising the problem
--

ns = [1, 3, 7, 10, 25, 50] :: [Int]
e' = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

-- An expression is a solution for a given list of numbers and a target if
-- the list of values in the expression is chosen from the list of numbers, and the
-- expression successfully evaluates to the target
{-
solution e' ns 765 -- True
-}
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

--
-- 9.6 - Brute-force solution
--

-- Return all possible ways of splitting a list into two non-empty lists that
-- append to give the original list
{-
split [1,2,3,4] -- [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
-}
split :: [a] -> [([a], [a])]
split []       = []
split [_     ] = []
split (x : xs) = ([x], xs) : [ (x : ls, rs) | (ls, rs) <- split xs ]

-- Return all possible expressions whose list of values is precisely a given list
{-
exprs [1,2,3] -- [1+(2+3),1-(2+3),1*(2+3),1/(2+3),1+(2-3),1-(2-3),1*(2-3),1/(2-3),
                  1+(2*3),1-(2*3),1*(2*3),1/(2*3),1+(2/3),1-(2/3),1*(2/3),1/(2/3),
                  (1+2)+3,(1+2)-3,(1+2)*3,(1+2)/3,(1-2)+3,(1-2)-3,(1-2)*3,(1-2)/3,
                  (1*2)+3,(1*2)-3,(1*2)*3,(1*2)/3,(1/2)+3,(1/2)-3,(1/2)*3,(1/2)/3]
-}
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns =
  [ e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r ]
 where
  combine l r = [ App o l r | o <- ops ]
  ops = [Add, Sub, Mul, Div]

-- Return all possible expressions made up from a list `ns` that solve for `n`
{-
solutions ns 765 -- [3*((7*(50-10))-25),
                     ((7*(50-10))-25)*3,
                     3*(((50-10)*7)-25), ...hundreds more...]
-}
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [ e | ns' <- choices ns, e <- exprs ns', eval e == [n] ]

--
-- 9.7 - Performance testing
--

-- All answers print in about 4.5 seconds. Can we do better?

{-
main :: IO ()
main = print (solutions ns 765)
-}

--
-- Combining generation and evaluation
--

type Result = (Expr, Int)

-- Return all possible results comprising expressions whose list of values is
-- precisely a given list
{-
results [1,2,3] -- [(1+(2+3),6),(1*(2+3),5),(1+(2*3),7),(1*(2*3),6),
                    ((1+2)+3,6),((1+2)*3,9),((1+2)/3,1),((1*2)+3,5),((1*2)*3,6)]
-}
results :: [Int] -> [Result]
results []  = []
results [n] = [ (Val n, n) | n > 0 ]
results ns =
  [ res
  | (ls, rs) <- split ns
  , lx       <- results ls
  , ry       <- results rs
  , res      <- combine' lx ry
  ]
 where
  combine' (l, x) (r, y) = [ (App o l r, apply o x y) | o <- ops, valid o x y ]
  ops = [Add, Sub, Mul, Div]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [ e | ns' <- choices ns, (e, m) <- results ns', m == n ]

-- Now it completes in about 1/3 second. Can we still do better??

main :: IO ()
main = print (solutions' ns 765)

--
-- Exploiting algebraic properties
--

-- See modifications to `valid` above

-- Completes in 0.06 seconds!

--
-- Exercises
--

-- 1
-- See above

-- 2
-- PASS

-- 3
-- PASS

-- 4
e4 = length $ exprs ns
-- Wrong answer - maybe because of modifications made to improve the performance?

-- 5
-- PASS

-- 6
-- PASS
