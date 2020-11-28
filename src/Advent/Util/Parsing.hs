module Advent.Util.Parsing (
    Parser
  , constrained
  , decimalInRange
  ) where

import Data.Void (Void)

import Text.Megaparsec
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
