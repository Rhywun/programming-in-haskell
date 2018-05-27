module Chapter07.Voting where

import           Data.List

--
-- First past the post
--
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- Counts the number of votes for candidate x
{-
count "Red" votes -- 2
-}
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- Removes duplicate values from a list
{-
rmdups votes -- ["Red","Blue","Green"]
-}
rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

-- Returns a list of vote counts per candidate, in increasing order of votes
{-
result votes -- [(1,"Green"),(2,"Red"),(3,"Blue")]
-}
result :: Ord a => [a] -> [(Int, a)]
result vs = sort [ (count v vs, v) | v <- rmdups vs ]

-- Returns the winner
{-
winner votes -- "Blue"
-}
winner :: Ord a => [a] -> a
winner = snd . last . result

--
-- Alternative vote
--
ballots :: [[String]]
ballots =
  [ ["Red", "Green"]
  , ["Blue"]
  , ["Green", "Red", "Blue"]
  , ["Blue", "Green", "Red"]
  , ["Green"]
  ]

-- Removes empty ballots from a list of ballots
{-
rmempty [[1],[],[2]] -- [[1],[2]]
-}
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

-- Eliminates a given candidate from each ballot in a list of ballots
{-
elim "Red" ballots -- [["Green"],["Blue"],["Green","Blue"],["Blue","Green"],["Green"]]
-}
elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

-- Ranks the first-choice candidates in a list of ballots, in increasing no. of votes
{-
rank ballots -- ["Red","Blue","Green"]
-}
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

-- Returns the winner
{-
winner' ballots -- "Green"
-}
winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
  []      -> undefined
  [c    ] -> c
  (c : _) -> winner' (elim c bs)
