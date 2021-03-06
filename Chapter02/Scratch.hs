module Chapter02.Scratch where

double x = x + x

quadruple x = double (double x)

factorial n = product [1 .. n]

average ns = sum ns `div` length ns

a = b + c
  where
    b = 1
    c = 2
