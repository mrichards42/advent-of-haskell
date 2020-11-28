module Advent.Y2019.Main where

import Advent.Util.Challenge (Challenge(..))
import qualified Advent.Util.CLI as CLI

year :: Int
year = 2019

challengeFns :: Challenge -> Maybe (String -> String)
challengeFns c =
  case c of
    _ -> Nothing

main :: IO ()
main = CLI.run year challengeFns
