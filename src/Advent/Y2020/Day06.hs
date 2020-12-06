{- Custom Customs -}
module Advent.Y2020.Day06 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import qualified Advent.Util.Parsing as P
import Data.List (foldl1')
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

{- Part 1

As your flight approaches the regional airport where you'll switch to a much
larger plane, customs declaration forms are distributed to the passengers.

The form asks a series of 26 yes-or-no questions marked a through z. All you
need to do is identify the questions for which anyone in your group answers
"yes". Since your group is just you, this doesn't take very long.

However, the person sitting next to you seems to be experiencing a language
barrier and asks if you can help. For each of the people in their group, you
write down the questions for which they answer "yes", one per line. For
example:

  abcx
  abcy
  abcz

In this group, there are 6 questions to which anyone answered "yes": a, b, c,
x, y, and z. (Duplicate answers to the same question don't count extra; each
question counts at most once.)

Another group asks for your help, then another, and eventually you've collected
answers from every group on the plane (your puzzle input). Each group's answers
are separated by a blank line, and within each group, each person's answers are
on a single line. For example:

  abc

  a
  b
  c

  ab
  ac

  a
  a
  a
  a

  b

This list represents answers from five groups:

  - The first group contains one person who answered "yes" to 3 questions: a,
    b, and c.
  - The second group contains three people; combined, they answered "yes" to 3
    questions: a, b, and c.
  - The third group contains two people; combined, they answered "yes" to 3
    questions: a, b, and c.
  - The fourth group contains four people; combined, they answered "yes" to
    only 1 question, a.
  - The last group contains one person who answered "yes" to only 1 question,
    b.

In this example, the sum of these counts is 3 + 3 + 3 + 1 + 1 = 11.

For each group, count the number of questions to which anyone answered "yes".
What is the sum of those counts?

-}

type CustomsForm = Set Char
type CustomsGroup = [CustomsForm]

pCustomsForm :: Parser (Set Char)
pCustomsForm = Set.fromList <$> M.some M.alphaNumChar

pCustomsGroup :: Parser CustomsGroup
pCustomsGroup = pCustomsForm `P.sepBy1NG` M.newline

parseInput :: String -> [CustomsGroup]
parseInput = parseOrError (pCustomsGroup `P.sepBy1NG` M.count 2 M.newline)

-- | >>> map solve1 $ parseInput "abcx\nabcy\nabcz\n\nabc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
-- [6, 3, 3, 3, 1, 1]
solve1 :: CustomsGroup -> Int
solve1 = Set.size . Set.unions

-- | >>> part1 <$> readFile "input/2020/day06.txt"
-- 6726
part1 :: String -> Int
part1 = sum . map solve1 . parseInput



{- Part 2

As you finish the last group's customs declaration, you notice that you misread
one word in the instructions:

You don't need to identify the questions to which anyone answered "yes"; you
need to identify the questions to which everyone answered "yes"!

Using the same example as above:

  abc

  a
  b
  c

  ab
  ac

  a
  a
  a
  a

  b

This list represents answers from five groups:

  - In the first group, everyone (all 1 person) answered "yes" to 3 questions:
    a, b, and c.
  - In the second group, there is no question to which everyone answered "yes".
  - In the third group, everyone answered yes to only 1 question, a. Since some
    people did not answer "yes" to b or c, they don't count.
  - In the fourth group, everyone answered yes to only 1 question, a.
  - In the fifth group, everyone (all 1 person) answered "yes" to 1 question,
    b.

In this example, the sum of these counts is 3 + 0 + 1 + 1 + 1 = 6.

For each group, count the number of questions to which everyone answered "yes".
What is the sum of those counts?

-}

-- | >>> map solve2 $ parseInput "abcx\nabcy\nabcz\n\nabc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
-- [3, 3, 0, 1, 1, 1]
solve2 :: CustomsGroup -> Int
solve2 = Set.size . foldl1' Set.intersection

-- | >>> part2 <$> readFile "input/2020/day06.txt"
-- 3316
part2 :: String -> Int
part2 = sum . map solve2 . parseInput
