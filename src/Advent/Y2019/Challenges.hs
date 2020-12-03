module Advent.Y2019.Challenges (challenges) where

import Advent.Util.Challenge (Challenge(..), ChallengeMap)
import qualified Data.Map as Map

-- Days
import qualified Advent.Y2019.Day01 as Day01
import qualified Advent.Y2019.Day02 as Day02
import qualified Advent.Y2019.Day03 as Day03
import qualified Advent.Y2019.Day04 as Day04
import qualified Advent.Y2019.Day05 as Day05
import qualified Advent.Y2019.Day06 as Day06
import qualified Advent.Y2019.Day07 as Day07

challenges :: ChallengeMap
challenges =
  Map.fromDistinctAscList [ (Challenge 2019 1 1, show . Day01.part1)
                          , (Challenge 2019 1 2, show . Day01.part2)
                          , (Challenge 2019 2 1, show . Day02.part1)
                          , (Challenge 2019 2 2, show . Day02.part2)
                          , (Challenge 2019 3 1, show . Day03.part1)
                          , (Challenge 2019 3 2, show . Day03.part2)
                          , (Challenge 2019 4 1, show . Day04.part1)
                          , (Challenge 2019 4 2, show . Day04.part2)
                          , (Challenge 2019 5 1, show . Day05.part1)
                          , (Challenge 2019 5 2, show . Day05.part2)
                          , (Challenge 2019 6 1, show . Day06.part1)
                          , (Challenge 2019 6 2, show . Day06.part2)
                          , (Challenge 2019 7 1, show . Day07.part1)
                          , (Challenge 2019 7 2, show . Day07.part2)
                          ]
