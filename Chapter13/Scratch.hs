{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Chapter13.Scratch where

import           Control.Applicative
import           Data.Char

--
-- 13.3 - Basic definitions
--

newtype Parser a = P (String -> [(a, String)])

-- Apply the given parser to a string
parse :: Parser a -> String -> [(a, String)]
parse (P p) = p


-- A parser which returns the first character of the input
{-
parse item "abc" -- [('a',"bc")]
parse item ""    -- []
-}
item :: Parser Char
item = P
  (\case
    []       -> []
    (x : xs) -> [(x, xs)]
  )

--
-- 13.4 - Sequencing parsers
--

{-
parse (fmap toUpper item) "abc" -- [('A',"bc")]
parse (fmap toUpper item) ""    -- []
-}
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
    []               -> []
    [(v, out)]       -> [(g v, out)]
    ((_, _) : _ : _) -> undefined) -- added to satisfy the linter

{-
parse (pure 1) "abc" -- [(1,"abc")]
-}
instance Applicative Parser where
  pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
    []               -> []
    [(g, out)]       -> parse (fmap g px) out
    ((_, _) : _ : _) -> undefined) -- added to satisfy the linter

-- A parser that consumes three characters, discards the second, and
-- returns the first and third as a pair
{-
parse three "ab"   -- []
parse three "abc"  -- [(('a','c'),"")]
parse three "abcd" -- [(('a','c'),"d")]
-}
three :: Parser (Char, Char)
three = g <$> item <*> item <*> item where g x _ z = (x, z)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
    []               -> []
    [(v, out)]       -> parse (f v) out
    ((_, _) : _ : _) -> undefined) -- added to satisfy the linter

{-
parse' three "ab"   -- []
parse' three "abc"  -- [(('a','c'),"")]
parse' three "abcd" -- [(('a','c'),"d")]
-}
three' :: Parser (Char, Char)
three' = do
  x <- item
  _ <- item
  z <- item
  return (x, z)

--
-- 13.5 - Making choices
--

{-
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

-- Laws:
empty <|> x == x
x <|> empty == x
x <|> (y <|> z) == (x <|> y) <|> z

-- An example instance:
instance Alternative Maybe where
  empty = Nothing
  Nothing <|> my = my
  (Just x) <|> _ = Just x
-}

{-
parse empty "abc"                  -- []
parse (item <|> return 'd') "abc"  -- [('a',"bc")]
parse (empty <|> return 'd') "abc" -- [('d',"abc")]
-}
instance Alternative Parser where
  empty :: Parser a
  empty = P (const [])

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
    []               -> parse q inp
    [(v, out)]       -> [(v, out)]
    ((_, _) : _ : _) -> undefined) -- added to satisfy the linter

--
-- 13.6 - Derived primitives
--

-- Returns a parser for single characters that satisfy the predicate p
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isLetter

alphanum :: Parser Char
alphanum = sat isAlphaNum

{-
parse (char 'a') "abc" -- [('a',"bc")]
-}
char :: Char -> Parser Char
char x = sat (== x)

-- Parses a string
{-
parse (string "abc") "abcdef" -- [("abc","def")]
parse (string "abc") "ab1234" -- []
-}
string :: String -> Parser String
string []       = return []
string (x : xs) = do
  _ <- char x
  _ <- string xs
  return (x : xs)

-- many/some - already defined in Alternative
{-
parse (many digit) "123abc" -- [("123","abc")]
parse (many digit) "abc"    -- [("","abc")]
parse (some digit) "abc"    -- []
-}

-- A parser for identifiers
{-
parse ident' "abc def" -- [("abc"," def")]
-}
ident' :: Parser String
ident' = do
  x  <- lower
  xs <- many alphanum
  return (x : xs)

-- A parser for natural numbers
{-
parse nat' "123abc" -- [(123,"abc")]
-}
nat' :: Parser Int
nat' = read <$> some digit

-- A parser that ignores whitespace
{-
parse space "123 abc" -- [((),"123 abc")]
-}
space :: Parser ()
space = do
  _ <- many (sat isSpace)
  return ()

-- A parser for integers
{-
parse int' "-123abc" -- [(-123,"abc")]
parse int' "123abc"  -- [(123,"abc")]
-}
int' :: Parser Int
int' =
  do
      _ <- char '-'
      n <- nat'
      return (-n)
    <|> nat'

--
-- 13.7 - Handling spacing
--

-- Returns a parser which ignores leading and trailing whitespace
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

ident :: Parser String
ident = token ident'

nat :: Parser Int
nat = token nat'

int :: Parser Int
int = token int'

symbol :: String -> Parser String
symbol xs = token (string xs)

-- A parser for a non-empty list of natural numbers [n1, n2, ... nx] that
-- ignores spacing around tokens
{-
parse nats " [1, 2, 3] " -- [([1,2,3],"")]
parse nats "[1,2,]"      -- []
-}
nats :: Parser [Int]
nats = do
  _  <- symbol "["
  n  <- nat
  ns <- many
    (do
      _ <- symbol ","
      nat
    )
  _ <- symbol "]"
  return (n : ns)

--
-- 13.8 - Arithmetic expressions
--

{-
A grammar which ensures precedence and right-associativity:
expr   ::== term (+ expr | ε)
term   ::== factor (* term | ε)
factor ::== (expr) | nat
nat    ::== 0 | 1 | 2 | ...
-}

expr :: Parser Int
expr = do
  t <- term
  (do
      _ <- symbol "+"
      e <- expr
      return (t + e)
    )
    <|> return t

term :: Parser Int
term = do
  f <- factor
  (do
      _ <- symbol "*"
      t <- term
      return (f * t)
    )
    <|> return f

factor :: Parser Int
factor =
  (do
      _ <- symbol "("
      e <- expr
      _ <- symbol ")"
      return e
    )
    <|> nat

{-
eval "2*3+4"        -- 10
eval "2 * (3 + 4)"  -- 14
eval "2 * 3 ^ 4"    -- *** Exception: Unused input ^ 4
eval "one plus two" -- *** Exception: Invalid input
-}
eval :: String -> Int
eval xs = case parse expr xs of
  [(n, [] )]           -> n
  [(_, out)]           -> errorWithoutStackTrace $ "Unused input " ++ out
  []                   -> errorWithoutStackTrace "Invalid input"
  ((_, []   ) : _ : _) -> undefined
  ((_, _ : _) : _ : _) -> undefined

--
-- 13.9 - Calculator
-- see Calculator.hs
--

