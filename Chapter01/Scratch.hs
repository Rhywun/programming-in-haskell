module Chapter01.Scratch where

double x = x + x

{-
sum' [1,2,3] -- 6
-}
sum' []       = 0
sum' (n : ns) = n + sum ns

{-
qsort [3,5,1,4,2]   -- [1,2,3,4,5]
qsort "Mississippi" -- "Miiiippssss"
-}
qsort []       = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
 where
  smaller = [ a | a <- xs, a <= x ]
  larger  = [ b | b <- xs, b > x ]

{-
seqn [getChar, getChar, getChar]
  > abc
  -- "abc"
-}
seqn []           = return []
seqn (act : acts) = do
  x  <- act
  xs <- seqn acts
  return (x : xs)
