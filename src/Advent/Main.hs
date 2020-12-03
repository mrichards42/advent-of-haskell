module Advent.Main where

import qualified Advent.Y2019.Challenges as Y2019
import qualified Advent.Y2020.Challenges as Y2020

import Advent.Util.Challenge (ChallengeMap, inputFile)
import qualified Advent.Util.CLI as CLI
import qualified Data.Map as Map
import System.Environment (getArgs, getProgName)
import System.TimeIt (timeIt)

allChallenges :: ChallengeMap
allChallenges = Map.unions [ Y2019.challenges
                           , Y2020.challenges
                           ]

main :: IO ()
main = do
  args <- getArgs
  if null args
     then getProgName >>= putStr . CLI.usage
     else case CLI.parseArgs challengeKeys args of
            Left errors -> mapM_ putStrLn errors
            Right challenges -> mapM_ run1 challenges
  where
    challengeKeys = Map.keys allChallenges
    run1 challenge =
      case Map.lookup challenge allChallenges of
        Just f -> do
          print challenge
          input <- readFile $ inputFile challenge
          timeIt $ putStr $ "  " ++ f input ++ "\n  "
        Nothing -> putStrLn $ "<Unknown challenge: " ++ show challenge ++ ">"
