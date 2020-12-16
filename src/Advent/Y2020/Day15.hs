{-# LANGUAGE BangPatterns #-}
{- Title -}
module Advent.Y2020.Day15 (part1, part2) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)

{- Part 1

-}

data GameState = GameState { gBoard :: IntMap [Int]
                           , gPrev :: Int
                           , gTurn :: Int
                           }
                           deriving (Show, Eq)

parseInput :: String -> [Int]
parseInput input = read $ "[" ++ input ++ "]"

gameFromList :: [Int] -> GameState
gameFromList input =
  let game = IM.fromList $ zip input (map return [1..])
      turn = length input + 1
  in GameState game (last input) turn

nextNumber1 :: GameState -> Int
nextNumber1 GameState{gBoard=board, gPrev=prev} =
  case IM.lookup prev board of
    Just (a:b:_) -> a - b -- seen more than once
    _ -> 0

-- | >>> map gPrev . take 8 . iterate step1 . gameFromList . parseInput $ "0,3,6"
-- [6,0,3,3,1,0,4,0]
step1 :: GameState -> GameState
step1 game@GameState{gTurn=turn, gBoard=board} =
  let !curr = nextNumber1 game
      board' = IM.alter (addTurn turn) curr board
  in GameState board' curr (turn + 1)
  where addTurn :: Int -> Maybe [Int] -> Maybe [Int]
        addTurn t old = let !new = take 2 $ t : fromMaybe [] old
                        in Just new

-- | >>> part1 "0,3,6"
-- 436
-- >>> timeIt =<< print . part1 <$> readFile "input/2020/day15.txt"
-- 203
part1 :: String -> Int
part1 input =
  let seed = parseInput input
  in gPrev . (!! (2020 - length seed)) . iterate step1 . gameFromList $ seed


{- Part 2

-}

runTurns :: Int -> GameState -> GameState
runTurns turns game
  | turns > 0 = let !game' = step1 game
                in runTurns (turns - 1) game'
  | otherwise = game

-- TODO: This is very slow (~90s), maybe use a Vector instead of a Map?

-- | >>> part2 <$> readFile "input/2020/day15.txt"
-- undefined
part2 :: String -> Int
part2 input =
  let seed = parseInput input
  in gPrev . runTurns (30000000 - length seed) . gameFromList $ seed
