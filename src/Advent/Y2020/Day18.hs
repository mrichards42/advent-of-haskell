{-# LANGUAGE TupleSections #-}

{- Operation Order -}
module Advent.Y2020.Day18 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import Data.List (foldl')
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

{- Part 1

As you look out the window and notice a heavily-forested continent slowly
appear over the horizon, you are interrupted by the child sitting next to you.
They're curious if you could help them with their math homework.

Unfortunately, it seems like this "math" follows different rules than you
remember.

The homework (your puzzle input) consists of a series of expressions that
consist of addition (+), multiplication (*), and parentheses ((...)). Just like
normal math, parentheses indicate that the expression inside must be evaluated
before it can be used by the surrounding expression. Addition still finds the
sum of the numbers on both sides of the operator, and multiplication still
finds the product.

However, the rules of operator precedence have changed. Rather than evaluating
multiplication before addition, the operators have the same precedence, and are
evaluated left-to-right regardless of the order in which they appear.

For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are as
follows:

    1 + 2 * 3 + 4 * 5 + 6
      3   * 3 + 4 * 5 + 6
          9   + 4 * 5 + 6
             13   * 5 + 6
                 65   + 6
                     71

Parentheses can override this order; for example, here is what happens if
parentheses are added to form 1 + (2 * 3) + (4 * (5 + 6)):

    1 + (2 * 3) + (4 * (5 + 6))
    1 +    6    + (4 * (5 + 6))
         7      + (4 * (5 + 6))
         7      + (4 *   11   )
         7      +     44
                51

Here are a few more examples:

  - 2 * 3 + (4 * 5) becomes 26.
  - 5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 437.
  - 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 12240.
  - ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 13632.

Before you can help with the homework, you need to understand it yourself.
Evaluate the expression on each line of the homework; what is the sum of the
resulting values?

-}


-- Expression evaluator --

data Op = Add | Mult
  deriving (Show)

data Expr = BinOp Op Expr Expr
          | Value Int

instance Show Expr where
  show (Value x) = show x
  show (BinOp Add  a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (BinOp Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

evalExpr :: Expr -> Int
evalExpr (Value a) = a
evalExpr (BinOp op a b) = evalOp op (evalExpr a) (evalExpr b)
  where evalOp Mult = (*)
        evalOp Add  = (+)


-- Common parsing --

pSpace :: Parser String
pSpace = M.many (M.char ' ')

pToken :: Char -> Parser Char
pToken c = M.char c <* pSpace

pOp :: Parser Op
pOp = M.choice [ Add <$ pToken '+'
               , Mult <$ pToken '*'
               ]

pValue :: Parser Expr
pValue = Value <$> L.decimal <* pSpace

foldOps :: Expr -> [(Op, Expr)] -> Expr
foldOps = foldl' compExpr

compExpr :: Expr -> (Op, Expr) -> Expr
compExpr a (op, b) = BinOp op a b


-- Part 1 parsing --

pSingle1 :: Parser Expr
pSingle1 = M.choice [ pToken '(' *> pExpr1 <* pToken ')'
                   , pValue
                   ]

pExpr1 :: Parser Expr
pExpr1 = foldOps <$> pSingle1 <*> M.many pBinOp
  where pBinOp :: Parser (Op, Expr)
        pBinOp = (,) <$> pOp <*> pSingle1

parseInput1 :: String -> [Expr]
parseInput1 = parseOrError $ pExpr1 `M.sepEndBy` M.newline

-- | >>> part1 "1 + (2 * 3) + (4 * (5 + 6))"
-- 51
-- >>> part1 "2 * 3 + (4 * 5)"
-- 26
-- >>> part1 "5 + (8 * 3 + 9 + 3 * 4 * 3)"
-- 437
-- >>> part1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
-- 12240
-- >>> part1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
-- 13632
-- >>> part1 <$> readFile "input/2020/day18.txt"
-- 31142189909908
part1 :: String -> Int
part1 = sum . map evalExpr . parseInput1


{- Part 2

You manage to answer the child's questions and they finish part 1 of their
homework, but get stuck when they reach the next section: advanced math.

Now, addition and multiplication have different precedence levels, but they're
not the ones you're familiar with. Instead, addition is evaluated before
multiplication.

For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are now
as follows:

    1 + 2 * 3 + 4 * 5 + 6
      3   * 3 + 4 * 5 + 6
      3   *   7   * 5 + 6
      3   *   7   *  11
         21       *  11
             231

Here are the other examples from above:

  - 1 + (2 * 3) + (4 * (5 + 6)) still becomes 51.
  - 2 * 3 + (4 * 5) becomes 46.
  - 5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 1445.
  - 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 669060.
  - ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 23340.

What do you get if you add up the results of evaluating the homework problems
using these new rules?

-}

pSingle2 :: Parser Expr
pSingle2 = M.choice [ pToken '(' *> pExpr2 <* pToken ')'
                    , pValue
                    ]

pExpr2 :: Parser Expr
pExpr2 = foldOps <$> pSingle2 <*> M.many pBinOp
  where pBinOp :: Parser (Op, Expr)
        pBinOp = do
          op <- pOp
          case op of
            Add  -> (Add,  ) <$> pSingle2  -- add takes the next simple value
            Mult -> (Mult, ) <$> pExpr2    -- mult takes the next full expr

parseInput2 :: String -> [Expr]
parseInput2 = parseOrError $ pExpr2 `M.sepEndBy` M.newline


-- | >>> part2 "1 + (2 * 3) + (4 * (5 + 6))"
-- 51
-- >>> part2 "2 * 3 + (4 * 5)"
-- 46
-- >>> part2 "5 + (8 * 3 + 9 + 3 * 4 * 3)"
-- 1445
-- >>> part2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
-- 669060
-- >>> part2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
-- 23340
-- >>> part2 <$> readFile "input/2020/day18.txt"
-- 323912478287549
part2 :: String -> Int
part2 = sum . map evalExpr . parseInput2
