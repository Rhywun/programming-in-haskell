{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Chapter13.Scratch where

import           Control.Applicative
import           Data.Char
import           System.IO

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
  -- Transform a value into a parser that always succeeds with this value as its result
  pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- Apply a parser that returns a function (pg) to a parser that returns an argument (px)
  -- to give a parser that returns the result of applying the function to the argument,
  -- and only succeeds if all the components succeed
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

-- Let's make Parser an instance of Monad so we can use the `do` syntax:

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
  -- The parser that always fails regardless of its input string
  empty :: Parser a
  empty = P (const [])

  -- Returns the result of the first parser if that succeeds, otherwise the result of the 2nd
  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
    []               -> parse q inp
    [(v, out)]       -> [(v, out)]
    ((_, _) : _ : _) -> undefined) -- added to satisfy the linter

--
-- 13.6 - Derived primitives
--

-- Returns a parser for single characters that satisfy the predicate p
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = satisfy isLetter

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

{-
parse (char 'a') "abc" -- [('a',"bc")]
-}
char :: Char -> Parser Char
char x = satisfy (== x)

-- Parses a given string
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

-- many (zero or more)
-- some (one or more)
-- Already defined in Alternative
{-
parse (many digit) "123abc" -- [("123","abc")]
parse (many digit) "abc"    -- [("","abc")]
parse (some digit) "abc"    -- []
-}

-- A parser for identifiers (a lowercase letter followed by zero or more alphanumerics)
{-
parse identifier' "abc def" -- [("abc"," def")]
-}
identifier' :: Parser String
identifier' = do
  x  <- lower
  xs <- many alphaNum
  return (x : xs)

-- A parser for natural numbers (one or more digits)
{-
parse natural' "123abc" -- [(123,"abc")]
-}
natural' :: Parser Int
natural' = read <$> some digit

-- A parser for spacing (zero or more whitespace characters)
{-
parse space "123 abc" -- [((),"123 abc")]
-}
space :: Parser ()
space = do
  _ <- many (satisfy isSpace)
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
      n <- natural'
      return (-n)
    <|> natural'

--
-- 13.7 - Handling spacing
--

-- Returns a parser which ignores leading and trailing whitespace after applying a parser (p)
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

{-
parse identifier " abc 123 " -- [("abc","123 ")]
-}
identifier :: Parser String
identifier = token identifier'

natural :: Parser Int
natural = token natural'

int :: Parser Int
int = token int'

symbol :: String -> Parser String
symbol xs = token (string xs)

-- A parser for a non-empty list of natural numbers [n1, n2, ... nx] that
-- ignores spacing around tokens
{-
parse naturals " [1, 2, 3] " -- [([1,2,3],"")]
parse naturals "[1,2,]"      -- []
-}
naturals :: Parser [Int]
naturals = do
  _  <- symbol "["
  n  <- natural
  ns <- many
    (do
      _ <- symbol ","
      natural
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
    <|> natural

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
--

-- IO utilities

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

beep :: IO ()
-- beep = putStr "\BEL"
beep = putStr "^"           -- Exercise 9

cls :: IO ()
cls = putStr "\ESC[2J"

writeAt :: (Int, Int) -> String -> IO ()
writeAt p xs = do
  goto p
  putStr xs

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Calculator

box :: [String]
box =
  [ "┌───────────────┐"
  , "│               │"
  , "├───┬───┬───┬───┤"
  , "│ Q │ C │ D │ = │"
  , "├───┼───┼───┼───┤"
  , "│ 1 │ 2 │ 3 │ + │"
  , "├───┼───┼───┼───┤"
  , "│ 4 │ 5 │ 6 │ - │"
  , "├───┼───┼───┼───┤"
  , "│ 7 │ 8 │ 9 │ * │"
  , "├───┼───┼───┼───┤"
  , "│ 0 │ ( │ ) │ / │"
  , "└───┴───┴───┴───┘"
  ]

buttons :: String
buttons = standard ++ extra
 where
  standard = "qcd=123+456-789*0()/"
  extra    = "QCD \ESC\BS\DEL\n"

showBox :: IO ()
showBox = sequence_ [ writeAt (1, y) b | (y, b) <- zip [1 ..] box ]

display :: String -> IO ()
display xs = do
  writeAt (3, 2) (replicate 13 ' ')
  writeAt (3, 2) (reverse (take 13 (reverse xs)))

-- Display the string xs, then get a character and process it if valid
calc :: String -> IO ()
calc xs = do
  display xs
  c <- getCh
  if c `elem` buttons
    then process c xs
    else do
      beep
      calc xs

process :: Char -> String -> IO ()
process c xs | c `elem` "qQ\ESC"    = quit
             | c `elem` "dD\BS\DEL" = delete xs
             | c `elem` "=\n"       = evaluate xs
             | c `elem` "cC"        = clear
             | otherwise            = press c xs

quit :: IO ()
quit = goto (1, 14) -- terminates here

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

evaluate :: String -> IO ()
evaluate xs = case parse expr xs of
  [(n, [])] -> calc (show n)
  _         -> do
    beep
    calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
  cls
  showBox
  clear

--
-- 13.11 - Exercises
--

-- 1

{-
parse comment "beep"               -- []
parse comment "-- i'm a comment\n" -- [((),"")]
-}
comment :: Parser ()
comment = do
  _ <- string "--"
  _ <- many (satisfy (/= '\n'))
  _ <- string "\n"
  return ()

-- 2
-- PASS

-- 3
-- PASS

-- 4
-- ?

-- 5
-- ?

-- 6

expr' :: Parser Int
expr' = do
  t <- term'
  (do
      _ <- symbol "+"
      e <- expr'
      return (t + e)
    )
    <|> (do
          _ <- symbol "-"
          e <- expr'
          return (t - e)
        )
    <|> return t

term' :: Parser Int
term' = do
  f <- factor'
  (do
      _ <- symbol "*"
      t <- term
      return (f * t)
    )
    <|> (do
          _ <- symbol "/"
          t <- term'
          return (f `div` t)
        )
    <|> return f

factor' :: Parser Int
factor' =
  (do
      _ <- symbol "("
      e <- expr
      _ <- symbol ")"
      return e
    )
    <|> int

-- 7
-- PASS

-- 8
-- PASS

-- 9
-- see attempt above - doesn't work
