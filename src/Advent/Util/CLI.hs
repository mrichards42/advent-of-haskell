-- | Common CLI functions
module Advent.Util.CLI (run, parseArgs, usage) where

import Advent.Util.Challenge (Challenge(..), allChallenges, inputFile)
import Advent.Util.Parsing (Parser, decimalInRange)
import Data.Either (partitionEithers)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs, getProgName)
import System.TimeIt (timeIt)
import Text.Megaparsec
import Text.Megaparsec.Char


usage :: Int -> String -> String
usage year progName =
  unlines [ "Usage: " ++ progName ++ " [all | dayXX... | dayXX.partYY...]"
          , "Runs one or more " ++ show year ++ " Advent Of Code puzzles."
          ]

-- This parsing code is almost certainly overkill, but I wanted to spend a
-- little time with Megaparsec.

data ParseResult = ChallengeList [Challenge]
                 | AllChallenges

{- | Challenge parser, for command line args.
    Handles the following formats:
    * [Day]XX.[part]YY  (a single part)
    * [Day]XX           (shorthand for both parts)
    * all               (shorthand for all defined challenges)
-}
pChallenge :: Parser ParseResult
pChallenge = allParser <|> challengeParser
  where
    allParser = do
      _ <- string' "all"
      return AllChallenges

    challengeParser = do
      _ <- optional (choice $ string' <$> ["day", "d"])
      day <- decimalInRange 1 25
      part <- optional $ do
        _ <- char '.'
        _ <- optional (choice $ string' <$> ["part", "p"])
        decimalInRange 1 2
      case part of
        Just n -> return $ ChallengeList [Challenge day n]
        _ -> return $ ChallengeList [Challenge day 1, Challenge day 2]

-- | CLI args parser
parseArgs :: [Challenge] -> [String] -> Either [String] (Set Challenge)
parseArgs knownChallenges args =
  case partitionEithers result of
    ([], rights) -> Right $ Set.fromList $ concat rights
    (lefts, _) -> Left lefts
  where
    result = [case parseMaybe pChallenge x of
                Just AllChallenges -> Right knownChallenges
                Just (ChallengeList cs) -> Right cs
                Nothing -> Left $ "Invalid challenge syntax: " ++ x
             | x <- args]

-- | The main entrypoint: run the CLI app for a given year
run :: Int -> (Challenge -> Maybe (String -> String)) -> IO ()
run year challengeFns = do
  args <- getArgs
  if null args
     then getProgName >>= putStr . usage year
     else case parseArgs knownChallenges args of
            Left errors -> mapM_ putStrLn errors
            Right challenges -> mapM_ run1 challenges
  where
    knownChallenges =
      filter (isJust . challengeFns) allChallenges
    run1 challenge =
      case challengeFns challenge of
        Just f -> do
          print challenge
          input <- readFile (inputFile year challenge)
          timeIt $ putStr $ "  " ++ f input ++ "\n  "
        Nothing -> putStrLn $ "<Unknown challenge: " ++ show challenge ++ ">"
