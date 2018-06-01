--
-- Screen utilities
--

-- Clear the screen
cls :: IO ()
cls = putStr "\ESC[2J"

-- A position on the screen, where (1,1) is the top-left corner
type Pos = (Int, Int)

-- Display a string at a given position
writeAt :: Pos -> String -> IO ()
writeAt p xs = do
  goto p
  putStr xs
  where goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

--
-- Game of life
--

width :: Int
width = 10

height :: Int
height = 10

-- A board is a list of positions at which there is a living cell
type Board = [Pos]

-- An example initial board
glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

-- Display a board on the screen
showCells :: Board -> IO ()
showCells board = sequence_ [ writeAt pos "O" | pos <- board ]

-- Is this cell alive?
isAlive :: Board -> Pos -> Bool
isAlive board pos = pos `elem` board

-- Is this cell empty?
isEmpty :: Board -> Pos -> Bool
isEmpty board pos = not (isAlive board pos)

-- Return the neighbors of a position
neighbors :: Pos -> [Pos]
neighbors (x, y) = map
  wrap
  [ (x - 1, y - 1)
  , (x    , y - 1)
  , (x + 1, y - 1)
  , (x - 1, y)
  , (x + 1, y)
  , (x - 1, y + 1)
  , (x    , y + 1)
  , (x + 1, y + 1)
  ]
  where wrap (x', y') = (((x' - 1) `mod` width) + 1, ((y' - 1) `mod` height) + 1)

-- Count number of live neighbors of a position
liveNeighbors :: Board -> Pos -> Int
liveNeighbors board = length . filter (isAlive board) . neighbors

-- List the survivors, those positions on the board that have 2 or 3 living neighbors
survivors :: Board -> [Pos]
survivors board = [ pos | pos <- board, liveNeighbors board pos `elem` [2, 3] ]

-- List the upcoming births, those positions on the board that are empty and
-- have 3 live neighbors
births :: Board -> [Pos]
births board =
  [ pos
  | pos <- rmDups (concatMap neighbors board)
  , isEmpty board pos
  , liveNeighbors board pos == 3
  ]
 where
  rmDups []       = []
  rmDups (x : xs) = x : rmDups (filter (/= x) xs)

-- Return the board's next generation
nextGen :: Board -> Board
nextGen board = survivors board ++ births board

-- Implement the game of life
life :: Board -> IO ()
life board = do
  cls
  showCells board
  wait (500000 :: Integer)
  life $ nextGen board
  where wait n = sequence_ [ return () | _ <- [1 .. n] ]

main :: IO ()
main = life glider

--
-- Issues
--

{-
1. Looks terrible when compiled and run in Terminal, with many missing cells
2. Can't get a bigger size to work properly at all
-}
