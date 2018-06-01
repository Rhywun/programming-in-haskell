module Chapter10.Exercises where

import           Data.Char
import           System.IO

-- 1

putStr' :: String -> IO ()
putStr' cs = sequence_ [ putChar c | c <- cs ]

-- 2
-- See Nim.hs

-- 3
-- See Nim.hs

-- 4, 5
-- 5 seems easier

-- Prompt for a digit and read a single character, with error checking
getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  putChar '\n'
  if isDigit x
    then return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt

adder :: IO ()
adder = do
  hSetBuffering stdout NoBuffering
  n  <- getDigit "How many numbers? "
  ns <- sequence [ getDigit ("Number " ++ show i ++ "? ") | i <- [1 .. n] ]
  putStr "The total is "
  print $ sum ns

-- 6

readLine :: IO String
readLine = do
  hSetBuffering stdout NoBuffering
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else if x == '\DEL'
      then do
        putChar '\DEL'
        putChar '\b'
        readLine               -- So close... but how do I discard the deleted chars??
      else do
        putChar x
        xs <- readLine
        return (x : xs)
 where
  getCh :: IO Char
  getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x
