module Advent.Util.Challenge (
    Challenge(..)
  , ChallengeMap
  , inputFile
  ) where

import Data.Map (Map)

-- | A Challenge is a (year, day, part) tuple
data Challenge = Challenge Int Int Int deriving (Eq, Ord, Show)

-- | A map of Challenge to a function implementing the challenge
type ChallengeMap = Map Challenge (String -> String)

-- | Returns the input file name for a Challenge in a given year
inputFile :: Challenge -> String
inputFile (Challenge year day _) =
  "input/" ++ show year ++ "/day" ++ pad 2 (show day) ++ ".txt"
  where pad n s = replicate (n - length s) '0' ++ s
