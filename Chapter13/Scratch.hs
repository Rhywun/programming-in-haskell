module Chapter13.Scratch where

import           Control.Applicative
import           Data.Char

--
-- 13.3 - Basic definitions
--

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

{-
parse item ""    -- []
parse item "abc" -- [('a',"bc")]
-}
item :: Parser Char
item = P
  (\inp -> case inp of
    []       -> []
    (x : xs) -> [(x, xs)]
  )

--
-- 13.4 - Sequencing parsers
--
