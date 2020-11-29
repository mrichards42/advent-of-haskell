module Advent.Y2019.Main where

import Advent.Util.Challenge (Challenge(..))
import qualified Advent.Util.CLI as CLI

import qualified Advent.Y2019.Day01 as Day01
import qualified Advent.Y2019.Day02 as Day02
import qualified Advent.Y2019.Day03 as Day03
import qualified Advent.Y2019.Day04 as Day04

year :: Int
year = 2019

challengeFns :: Challenge -> Maybe (String -> String)
challengeFns c =
  case c of
    (Challenge 1 1) -> Just $ show . Day01.part1
    (Challenge 1 2) -> Just $ show . Day01.part2
    (Challenge 2 1) -> Just $ show . Day02.part1
    (Challenge 2 2) -> Just $ show . Day02.part2
    (Challenge 3 1) -> Just $ show . Day03.part1
    (Challenge 3 2) -> Just $ show . Day03.part2
    (Challenge 4 1) -> Just $ show . Day04.part1
    (Challenge 4 2) -> Just $ show . Day04.part2
    _ -> Nothing

main :: IO ()
main = CLI.run year challengeFns
