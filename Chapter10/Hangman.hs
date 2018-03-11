module Chapter10.Hangman where

import System.IO

main :: IO ()
main = do putStrLn "Player one, think of a word: "
          word <- sgetLine
          putStrLn "Player two, try to guess the word!"
          play word

-- Like `getLine`, except it echoes each character as a dash symbol '-'
sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)
          where getCh :: IO Char
                getCh = do hSetEcho stdin False
                           x <- getChar
                           hSetEcho stdin True
                           return x

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then putStrLn "You got it!!"
               else do putStrLn (match word guess)
                       play word
                    where match :: String -> String -> String
                          match xs ys = [if x `elem` ys then x else '-' | x <- xs]
