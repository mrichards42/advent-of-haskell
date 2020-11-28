module Advent.Y2019.Main where

import Advent.Util.Challenge (Challenge(..))
import qualified Advent.Util.CLI as CLI

import qualified Advent.Y2019.Day01 as Day01

year :: Int
year = 2019

challengeFns :: Challenge -> Maybe (String -> String)
challengeFns c =
  case c of
    (Challenge 1 1) -> Just $ show . Day01.part1
    (Challenge 1 2) -> Just $ show . Day01.part2
    _ -> Nothing

main :: IO ()
main = CLI.run year challengeFns
