module Chapter08.Machine where

-- An expression is either an integer value or the sum or product of two other expressions
data Expr = Val Int | Add Expr Expr | Mult Expr Expr deriving Show

e1 = Add (Add (Val 2) (Val 3)) (Val 4)
e2 = Add (Mult (Val 2) (Val 3)) (Mult (Val 4) (Val 5))

-- First stab - not an "abstract machine" because order of eval is determined by Haskell
{-
value' e1 -- 9
value' e2 -- 26
-}
value' :: Expr -> Int
value' (Val n   ) = n
value' (Add  x y) = value' x + value' y
value' (Mult x y) = value x * value y

--

-- An operation is either "evaluate an expression" or "add/multiply an integer"
data Op = EVAL_ADD Expr | ADD Int | EVAL_MULT Expr | MULT Int

-- A control stack is a list of operations
type Cont = [Op]

-- Evaluate an expression in the context of a control stack
eval :: Expr -> Cont -> Int
eval (Val n   ) c = exec c n
eval (Add  x y) c = eval x (EVAL_ADD y : c)
eval (Mult x y) c = eval x (EVAL_MULT y : c)

-- Execute a control stack in the context of an integer argument
exec :: Cont -> Int -> Int
exec []                n = n
exec (EVAL_ADD  y : c) n = eval y (ADD n : c)
exec (ADD       n : c) m = exec c (n + m)
exec (EVAL_MULT y : c) n = eval y (MULT n : c)
exec (MULT      n : c) m = exec c (n * m)

-- Evaluate an expression to an integer
{-
value e1 -- 9
value e2 -- 26
-}
value :: Expr -> Int
value e = eval e []
