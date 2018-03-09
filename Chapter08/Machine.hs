module Chapter08.Machine where

-- An expression is either an integer value or the sum of two other expressions
-- E.g. Add (Add (Val 2) (Val 3)) (Val 4)
data Expr = Val Int | Add Expr Expr deriving Show

-- An operation is either "evaluate an expression" or "add an integer"
data Op = EVAL Expr | ADD Int

-- A control stack is a list of operations
type Cont = [Op]

-- Evaluate an expression in the context of a control stack
eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

-- Execute a control stack in the context of an integer argument
exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n + m)

-- Evaluate an expression to an integer
-- E.g. value (Add (Add (Val 2) (Val 3)) (Val 4)) == 9
value :: Expr -> Int
value e = eval e []
