module Chapter10.Scratch where

--
-- 10.4 - Sequencing
--

act :: IO (Char, Char)
act = do
  x <- getChar
  _ <- getChar
  y <- getChar
  return (x, y)

--
-- 10.5 - Derived primitives
--

strlen :: IO ()
strlen = do
  putStr "Enter a string: "
  xs <- getLine
  putStr "The string has "
  putStr (show (length xs))
  putStrLn " characters."
