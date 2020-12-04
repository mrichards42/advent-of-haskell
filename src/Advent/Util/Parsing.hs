module Advent.Util.Parsing (
    Parser
  , constrained
  , decimalInRange
  , decimalOneOf
  , parseOrError
  , sepByNG
  , sepBy1NG
  , sepEndByNG
  , sepEndBy1NG
  , stringChoice
  ) where

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

{- | Adds a constraint to a parser
 If the constraint succeeds, returns the parser result.
 If the constraint fails, fails with an error message.
-}
constrained :: (a -> Bool) -> String -> Parser a -> Parser a
constrained f msg parser = do
  o <- getOffset
  x <- parser
  if f x
     then return x
     else do
       setOffset o
       fail msg

-- | Parses a decimal in the given range
decimalInRange :: (Show a, Ord a, Num a) => a -> a -> Parser a
decimalInRange minN maxN =
  constrained
    (\n -> minN <= n && n <= maxN)
    ("expected integer between " ++ show minN ++ " and " ++ show maxN)
    L.decimal

-- | Parses a decimal that is one of a list of options
decimalOneOf :: (Show a, Ord a, Num a) => [a] -> Parser a
decimalOneOf options =
  constrained
    (`elem` options)
    ("expected one of: " ++ show options)
    L.decimal

-- | Parses or errors, allowing unlimited trailing newlines
parseOrError :: Parser a -> String -> a
parseOrError p s =
  case parse (p <* many newline <* eof) "" s of
    Right result -> result
    Left err -> error $ errorBundlePretty err

-- | Choice of strings
stringChoice :: [String] -> Parser String
stringChoice = choice . map string

-- | Non-greedy sepBy
sepByNG :: Parser a -> Parser sep -> Parser [a]
sepByNG p sep = sepBy1NG p sep <|> pure []

-- | Non-greedy sepBy1
sepBy1NG :: Parser a -> Parser sep -> Parser [a]
sepBy1NG p sep = (:) <$> p <*> many (try (sep *> p))

-- | Non-greedy sepEndBy
sepEndByNG :: Parser a -> Parser sep -> Parser [a]
sepEndByNG p sep = sepEndBy1NG p sep <|> pure []

-- | Non-greedy sepEndBy1
sepEndBy1NG :: Parser a -> Parser sep -> Parser [a]
sepEndBy1NG p sep = sepBy1NG p sep <* optional sep
