-- | Common CLI functions
module Advent.Util.CLI (parseArgs, usage) where

import Advent.Util.Challenge (Challenge(..))
import Advent.Util.Parsing (Parser, decimalInRange, decimalOneOf)
import Data.Either (partitionEithers)
import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char


usage :: String -> String
usage progName =
  unlines [ "Usage: " ++ progName ++ " year [all | dayXX... | dayXX.partYY...]"
          , "Runs one or more Advent Of Code puzzles."
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
pChallenge :: Int -> Parser ParseResult
pChallenge year = allParser <|> challengeParser
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
        Just n -> return $ ChallengeList [Challenge year day n]
        _ -> return $ ChallengeList [Challenge year day 1, Challenge year day 2]

pYear :: [Int] -> Parser Int
pYear = decimalOneOf

challengeYear :: Challenge -> Int
challengeYear (Challenge y _ _) = y

parseChallenges :: Int -> [Challenge] -> [String] -> Either [String] (Set Challenge)
parseChallenges year allChallenges args =
  case partitionEithers result of
    ([], rights) -> Right $ Set.fromList $ concat rights
    (lefts, _) -> Left lefts
  where
    challengesInYear = filter ((== year) . challengeYear) allChallenges
    result = map parse1 args
    parse1 arg = case parseMaybe (pChallenge year) arg of
                   Just AllChallenges -> Right challengesInYear
                   Just (ChallengeList cs) -> Right cs
                   Nothing -> Left $ "Invalid challenge syntax: " ++ arg

-- | CLI args parser
parseArgs :: [Challenge] -> [String] -> Either [String] (Set Challenge)
parseArgs allChallenges (yearArg:challengeArgs) =
  case parseMaybe (pYear validYears) yearArg of
    Nothing -> Left ["Expected year to be one of " ++ show validYears]
    Just year -> parseChallenges year allChallenges challengeArgs
  where validYears = nub $ map challengeYear allChallenges
parseArgs _ [] = Left ["No args"]
