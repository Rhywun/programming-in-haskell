import Data.Char
import Data.List
import System.IO

--
-- Basic declarations
--

size = 3 :: Int

type Grid = [[Player]]  -- Inner and outer lists assumed to have length `size`

-- X or O or B (for "blank")
-- The ordering here is important for the unbeatable "minimax" algorithm
data Player = O | B | X deriving (Eq, Ord, Show)

-- An example winning grid
g = [[B,O,O],[O,X,O],[X,X,X]] :: Grid

-- An example draw
g' = [[O,O,X],[X,X,O],[O,X,O]] :: Grid

next :: Player -> Player
next O = X
next B = B    -- Included only for completeness
next X = O

--
-- Grid utilities
--

-- E.g. empty == [[B,B,B],[B,B,B],[B,B,B]]
empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = notElem B . concat

-- Whose turn is it?
turn :: Grid -> Player
turn grid = if os <= xs then O else X
            where os = length (filter (== O) ps)
                  xs = length (filter (== X) ps)
                  ps = concat grid

-- Has this player won?
-- E.g wins X g == True
wins :: Player -> Grid -> Bool
wins player grid = any line (rows ++ cols ++ diags)
                   where line = all (== player)
                         rows = grid
                         cols = transpose grid
                         diags = [diag grid, diag (map reverse grid)]
                         diag :: Grid -> [Player]
                         diag grid = [grid !! n !! n | n <- [0..size - 1]]

-- Has either player won?
won :: Grid -> Bool
won grid = wins O grid || wins X grid

--
-- Displaying a grid
--

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size * 4) - 1) '-']
                showRow = beside . interleave bar . map showPlayer
                          where beside = foldr1 (zipWith (++))
                                bar    = replicate 3 "|"

-- SKIP
