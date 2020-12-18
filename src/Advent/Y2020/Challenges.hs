module Advent.Y2020.Challenges (challenges) where

import Advent.Util.Challenge (Challenge(..), ChallengeMap)
import qualified Data.Map as Map

-- Days
import qualified Advent.Y2020.Day01 as Day01
import qualified Advent.Y2020.Day02 as Day02
import qualified Advent.Y2020.Day03 as Day03
import qualified Advent.Y2020.Day04 as Day04
import qualified Advent.Y2020.Day05 as Day05
import qualified Advent.Y2020.Day06 as Day06
import qualified Advent.Y2020.Day07 as Day07
import qualified Advent.Y2020.Day08 as Day08
import qualified Advent.Y2020.Day09 as Day09
import qualified Advent.Y2020.Day10 as Day10
import qualified Advent.Y2020.Day11 as Day11
import qualified Advent.Y2020.Day12 as Day12
import qualified Advent.Y2020.Day13 as Day13
import qualified Advent.Y2020.Day14 as Day14
import qualified Advent.Y2020.Day15 as Day15
import qualified Advent.Y2020.Day16 as Day16
import qualified Advent.Y2020.Day17 as Day17

challenges :: ChallengeMap
challenges =
  Map.fromDistinctAscList [ (Challenge 2020  1 1, show . Day01.part1)
                          , (Challenge 2020  1 2, show . Day01.part2)
                          , (Challenge 2020  2 1, show . Day02.part1)
                          , (Challenge 2020  2 2, show . Day02.part2)
                          , (Challenge 2020  3 1, show . Day03.part1)
                          , (Challenge 2020  3 2, show . Day03.part2)
                          , (Challenge 2020  4 1, show . Day04.part1)
                          , (Challenge 2020  4 2, show . Day04.part2)
                          , (Challenge 2020  5 1, show . Day05.part1)
                          , (Challenge 2020  5 2, show . Day05.part2)
                          , (Challenge 2020  6 1, show . Day06.part1)
                          , (Challenge 2020  6 2, show . Day06.part2)
                          , (Challenge 2020  7 1, show . Day07.part1)
                          , (Challenge 2020  7 2, show . Day07.part2)
                          , (Challenge 2020  8 1, show . Day08.part1)
                          , (Challenge 2020  8 2, show . Day08.part2)
                          , (Challenge 2020  9 1, show . Day09.part1)
                          , (Challenge 2020  9 2, show . Day09.part2)
                          , (Challenge 2020 10 1, show . Day10.part1)
                          , (Challenge 2020 10 2, show . Day10.part2)
                          , (Challenge 2020 11 1, show . Day11.part1)
                          , (Challenge 2020 11 2, show . Day11.part2)
                          , (Challenge 2020 12 1, show . Day12.part1)
                          , (Challenge 2020 12 2, show . Day12.part2)
                          , (Challenge 2020 13 1, show . Day13.part1)
                          , (Challenge 2020 13 2, show . Day13.part2)
                          , (Challenge 2020 14 1, show . Day14.part1)
                          , (Challenge 2020 14 2, show . Day14.part2)
                          , (Challenge 2020 15 1, show . Day15.part1)
                          , (Challenge 2020 15 2, show . Day15.part2)
                          , (Challenge 2020 16 1, show . Day16.part1)
                          , (Challenge 2020 16 2, show . Day16.part2)
                          , (Challenge 2020 17 1, show . Day17.part1)
                          , (Challenge 2020 17 2, show . Day17.part2)
                          ]
