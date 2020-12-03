module Advent.Y2020.Challenges (challenges) where

import Advent.Util.Challenge (Challenge(..), ChallengeMap)
import qualified Data.Map as Map

-- Days
import qualified Advent.Y2020.Day01 as Day01
import qualified Advent.Y2020.Day02 as Day02

challenges :: ChallengeMap
challenges =
  Map.fromDistinctAscList [ (Challenge 2020 1 1, show . Day01.part1)
                          , (Challenge 2020 1 2, show . Day01.part2)
                          , (Challenge 2020 2 1, show . Day02.part1)
                          , (Challenge 2020 2 2, show . Day02.part2)
                          ]
