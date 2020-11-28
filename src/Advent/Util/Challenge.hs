module Advent.Util.Challenge (
    Challenge(..)
  , allChallenges
  , inputFile
  ) where

-- | A Challenge is a (day, part) tuple
data Challenge = Challenge Int Int deriving (Eq, Ord, Show)

-- | Every possible challenge in a year: days 1-25; parts 1-2
allChallenges :: [Challenge]
allChallenges = [Challenge day part | day <- [1..25] , part <- [1..2]]

-- | Returns the input file name for a Challenge in a given year
inputFile :: Int -> Challenge -> String
inputFile year (Challenge day _) =
  "input/" ++ show year ++ "/day" ++ pad 2 (show day) ++ ".txt"
  where pad n s = replicate (n - length s) '0' ++ s
