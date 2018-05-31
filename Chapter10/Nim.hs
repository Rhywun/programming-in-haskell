module Chapter10.Nim where

import           Data.Char
import           System.IO

--
-- Game utilities
--

-- Return the next player (1 or 2)
next :: Int -> Int
next 1 = 2
next 2 = 1
next _ = undefined

type Board = [Int]

-- The initial board:
{-
1 : * * * * *
2 : * * * *
3 : * * *
4 : * *
5 : *
-}
initial :: Board
initial = [5, 4, 3, 2, 1]

-- Is the game finished?
finished :: Board -> Bool
finished = all (== 0)

-- Is this move valid?
{-
valid initial 1 3 -- True                   <- The 1st row has 5 stars
valid initial 4 3 -- False                  <- The 4th row only has 2 stars
-}
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

-- Return a new board showing the results of a valid move on a board
{-
move initial 1 3 -- [2,4,3,2,1]             <- Remove 3 stars from the 1st row
-}
move :: Board -> Int -> Int -> Board
move board row num = [ update r n | (r, n) <- zip [1 ..] board ]
  where update r n = if r == row then n - num else n

--
-- IO utilities
--

newline :: IO ()
newline = putChar '\n'

-- Display a row on the screen
{-
putRow 1 5 -- => displays "1: * * * * *"
-}
putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

-- Display a board on the screen
{-
putBoard initial -- => displays all five rows (initial == [5,4,3,2,1])
-}
putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do
  putRow 1 a
  putRow 2 b
  putRow 3 c
  putRow 4 d
  putRow 5 e
putBoard _ = undefined

-- Prompt for a digit and read a single character, with error checking
getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
    then return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt

--
-- Game of nim
--

play :: Board -> Int -> IO ()
play board player = do
  newline
  putBoard board                      -- <-- Exercises 2 and 3
  if finished board
    then do
      newline
      putStr "Player "
      putStr (show (next player))
      putStrLn " wins!!"
    else do
      newline
      putStr "Player "
      print player
      row <- getDigit "Enter a row number: "
      num <- getDigit "Stars to remove: "
      if valid board row num
        then play (move board row num) (next player)
        else do
          newline
          putStrLn "ERROR: Invalid move"
          play board player

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play initial 1

-- Exercise 2

putBoard' :: Board -> IO ()
putBoard' xs = go xs 1
 where
  go []         _ = return ()
  go (x' : xs') n = do
    putRow n x'
    go xs' $ n + 1

-- Exercise 3

putBoard'' :: Board -> IO ()
putBoard'' board = sequence_ [ putRow n row | (n, row) <- zip [1 ..] board ]
