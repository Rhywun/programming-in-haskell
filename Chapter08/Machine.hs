module Chapter08.Machine where

-- An expression is either an integer value or the sum or product of two other expressions
-- E.g. Add (Add (Val 2) (Val 3)) (Val 4)
data Expr = Val Int | Add Expr Expr | Mult Expr Expr deriving Show

e = Add (Add (Val 2) (Val 3)) (Val 4)
e' = Add (Mult (Val 2) (Val 3)) (Mult (Val 4) (Val 5))

-- An operation is either "evaluate an expression" or "add/multiply an integer"
data Op = EVAL_ADD Expr | ADD Int | EVAL_MULT Expr | MULT Int

-- A control stack is a list of operations
type Cont = [Op]

-- Evaluate an expression in the context of a control stack
eval :: Expr -> Cont -> Int
eval (Val n)   c  = exec c n
eval (Add x y) c  = eval x (EVAL_ADD y : c)
eval (Mult x y) c = eval x (EVAL_MULT y : c)

-- Execute a control stack in the context of an integer argument
exec :: Cont -> Int -> Int
exec [] n                = n
exec (EVAL_ADD y : c) n  = eval y (ADD n : c)
exec (ADD n : c)  m      = exec c (n + m)
exec (EVAL_MULT y : c) n = eval y (MULT n : c)
exec (MULT n : c) m      = exec c (n * m)

-- Evaluate an expression to an integer
-- E.g. value e == 9
-- E.g. value e' == 26
value :: Expr -> Int
value e = eval e []
