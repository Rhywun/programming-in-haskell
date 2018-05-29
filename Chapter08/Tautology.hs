module Chapter08.Tautology where

--
-- Propositions
--

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop deriving Show

p1 = And (Var 'A') (Not (Var 'A'))
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
p5 = Or (Var 'A') (Var 'A')

--
-- Substitutions
--

type Assoc k v = [(k,v)]

-- A lookup table which associates variable names to boolean values
-- E.g. [('A',False),('B',True)]
type Subst = Assoc Char Bool

-- Finds the value in a lookup table `t` for key `k`
-- E.g. find 'A' [('A',False),('B',True)] == False
find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k' ]

--
-- Tautology checker
--

-- Evaluate a proposition given a substitution for its variables
-- E.g. eval [('A',False),('B',True)] p2 == True
eval :: Subst -> Prop -> Bool
eval _ (Const b  ) = b
eval s (Var   x  ) = find x s
eval s (Not   p  ) = not (eval s p)
eval s (And   p q) = eval s p && eval s q
eval s (Or    p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q

-- Return a list of all the variables in a proposition
-- E.g. vars p1 == ['A','A']
-- E.g. vars p2 == ['A','B','A']
vars :: Prop -> [Char]
vars (Const _  ) = []
vars (Var   x  ) = [x]
vars (Not   p  ) = vars p
vars (And   p q) = vars p ++ vars q
vars (Or    p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- Return a list of all possbile combinations of logical values of a given length
-- E.g. bools 2 == [[False,False],[False,True],[True,False],[True,True]]
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss where bss = bools (n - 1)

-- Returns a list with duplicate values removed
-- E.g. rmdups (vars p1) == ['A']
rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

-- Return all possible substitutions for a proposition `p`
-- E.g. substs p1 == [[('A',False)],[('A',True)]]
-- E.g. substs p2 == [[('A',False),('B',False)],[('A',False),('B',True)],
--                    [('A',True),('B',False)],[('A',True),('B',True)]]
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs)) where vs = rmdups (vars p)

-- Decide if a proposition is a tautology, by checking if it evaluates to True for
-- all possible substitutions
isTaut :: Prop -> Bool
isTaut p = and [ eval s p | s <- substs p ]
