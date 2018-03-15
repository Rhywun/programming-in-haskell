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

g1 = [[B,O,O],[O,X,O],[X,X,X]] :: Grid    -- A not-full grid, X wins
g2 = [[O,O,X],[X,X,O],[O,X,O]] :: Grid    -- A full grid, nobody wins

-- Who is the next player after the given player?
next :: Player -> Player
next O = X
next B = B    -- Included only for completeness
next X = O

--
-- Grid utilities
--

-- Return a fresh, empty grid
-- E.g. empty == [[B,B,B],[B,B,B],[B,B,B]]
empty :: Grid
empty = replicate size (replicate size B)

-- Is the grid full?
-- E.g. full g1 == False
--      full g2 == True
full :: Grid -> Bool
full = notElem B . concat

-- Whose turn is it?
-- E.g. turn empty == O
--      turn g1 = O
turn :: Grid -> Player
turn grid = if os <= xs then O else X
            where os = length (filter (== O) ps)
                  xs = length (filter (== X) ps)
                  ps = concat grid

-- Has this player won? Return whether the player owns a contiguous row, column, or diagonal
-- E.g wins X g1 == True
wins :: Player -> Grid -> Bool
wins player grid = any line (rows ++ cols ++ diags)
                   where line = all (== player)
                         rows = grid
                         cols = transpose grid
                         diags = [diag grid, diag (map reverse grid)]
                         diag grid = [grid !! n !! n | n <- [0..size - 1]]

-- Has either player won?
-- E.g. won g1 == True
--      won g2 == False
won :: Grid -> Bool
won grid = wins O grid || wins X grid

--
-- Displaying a grid
--

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size * 4) - 1) '-']

-- E.g. showRow [X,O,X] == ["   |   |   "," X | O | X ","   |   |   "]
showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where beside = foldr1 (zipWith (++))
                bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

-- Return a new list with a value interleaved between each element of a specified list
-- E.g. interleave '-' "hello" == "h-e-l-l-o"
interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

--
-- Making a move
--

-- Is a move valid? That is, within the grid and the specified cell is Blank
-- E.g. valid g1 0 == True        (position 0 is the top-left corner)
--      valid g2 0 == False
valid :: Grid -> Int -> Bool
valid grid i = 0 <= i && i < size^2 && concat grid !! i == B

-- Return a new grid reflecting the result of a player making a move at position `i`.
-- Singleton list is a successful move, empty list denotes failure
-- E.g. move g1 0 $ turn g1 == [[[O,O,O],[O,X,O],[X,X,X]]]
--      move g1 1 $ turn g1 == []
move :: Grid -> Int -> Player -> [Grid]
move grid i player = [chop size (xs ++ [player] ++ ys) | valid grid i]
                     where (xs,B:ys) = splitAt i (concat grid)

-- Break a list into maximal segments of a given length
-- e.g. chop 2 "helloworld" == ["he","ll","ow","or","ld"]
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

--
-- Reading a number
--

-- Read a number from stdin, with error checking
-- (Note: support for deleting with Backspace not included)
getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs)
                   else
                     do putStrLn "ERROR: Invalid number"
                        getNat prompt

--
-- Human vs human
--

tictactoe :: IO ()
tictactoe = run empty O

-- An action that implements the game using two mutually recursive functions
-- `run` and `run'`

run :: Grid -> Player -> IO ()
run grid player = do cls
                     goto (1, 1)
                     putGrid grid
                     run' grid player
                  where cls = putStr "\ESC[2J"
                        goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run' :: Grid -> Player -> IO ()
run' grid player | wins O grid = putStrLn "Player O wins!\n"
                 | wins X grid = putStrLn "Player X wins!\n"
                 | full grid   = putStrLn "It's a draw!\n"
                 | otherwise   = do i <- getNat (prompt player)
                                    case move grid i player of
                                      []      -> do putStrLn "ERROR: Invalid move"
                                                    run' grid player
                                      [grid'] -> run grid' (next player)
                 where prompt player = "Player " ++ show player ++ ", enter your move: "

--
-- Game trees
--

-- cont. p. 147
