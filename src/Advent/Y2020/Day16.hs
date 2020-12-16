{- Ticket Translation -}
module Advent.Y2020.Day16 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import Advent.Util.Function (fixedPoint)
import qualified Advent.Util.Parsing as P
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

{- Part 1

As you're walking to yet another connecting flight, you realize that one of the
legs of your re-routed trip coming up is on a high-speed train. However, the
train ticket you were given is in a language you don't understand. You should
probably figure out what it says before you get to the train station after the
next flight.

Unfortunately, you can't actually read the words on the ticket. You can,
however, read the numbers, and so you figure out the fields these tickets must
have and the valid ranges for values in those fields.

You collect the rules for ticket fields, the numbers on your ticket, and the
numbers on other nearby tickets for the same train service (via the airport
security cameras) together into a single document you can reference (your
puzzle input).

The rules for ticket fields specify a list of fields that exist somewhere on
the ticket and the valid ranges of values for each field. For example, a rule
like class: 1-3 or 5-7 means that one of the fields in every ticket is named
class and can be any value in the ranges 1-3 or 5-7 (inclusive, such that 3 and
5 are both valid in this field, but 4 is not).

Each ticket is represented by a single line of comma-separated values. The
values are the numbers on the ticket in the order they appear; every ticket has
the same format. For example, consider this ticket:

    .--------------------------------------------------------.
    | ????: 101    ?????: 102   ??????????: 103     ???: 104 |
    |                                                        |
    | ??: 301  ??: 302             ???????: 303      ??????? |
    | ??: 401  ??: 402           ???? ????: 403    ????????? |
    '--------------------------------------------------------'

Here, ? represents text in a language you don't understand. This ticket might
be represented as 101,102,103,104,301,302,303,401,402,403; of course, the
actual train tickets you're looking at are much more complicated. In any case,
you've extracted just the numbers in such a way that the first number is always
the same specific field, the second number is always a different specific
field, and so on - you just don't know what each position actually means!

Start by determining which tickets are completely invalid; these are tickets
that contain values which aren't valid for any field. Ignore your ticket for
now.

For example, suppose you have the following notes:

    class: 1-3 or 5-7
    row: 6-11 or 33-44
    seat: 13-40 or 45-50

    your ticket:
    7,1,14

    nearby tickets:
    7,3,47
    40,4,50
    55,2,20
    38,6,12

It doesn't matter which position corresponds to which field; you can identify
invalid nearby tickets by considering only whether tickets contain values that
are not valid for any field. In this example, the values on the first nearby
ticket are all valid for at least one field. This is not true of the other
three nearby tickets: the values 4, 55, and 12 are are not valid for any field.
Adding together all of the invalid values produces your ticket scanning error
rate: 4 + 55 + 12 = 71.

Consider the validity of the nearby tickets you scanned. What is your ticket
scanning error rate?

-}

-- Parsing --

type Ticket = [Int]

data Rule = Rule String IntSet
  deriving (Eq, Show, Ord)

-- | >>> parseOrError pRule "departure location: 31-538 or 546-960"
-- Rule "departure location" (IS.fromList $ [31..538] ++ [546..960])
pRule :: Parser Rule
pRule = do
  name <- M.takeWhile1P (Just "rule name") (/= ':')
  _ <- M.string ": "
  ranges <- pRange `M.sepBy` M.string " or "
  return $ Rule name (IS.unions ranges)
  where pRange :: Parser IntSet
        pRange = do
          a <- L.decimal
          _ <- M.char '-'
          b <- L.decimal
          return $ IS.fromList [a..b]

pRules :: Parser [Rule]
pRules = pRule `P.sepBy1NG` M.newline

pTicket :: Parser [Int]
pTicket = L.decimal `P.sepBy1NG` M.char ','

pInput :: Parser ([Rule], Ticket, [Ticket])
pInput = do
  rules <- pRules <* M.some M.newline
  _ <- M.string "your ticket:\n"
  yourTicket <- pTicket <* M.some M.newline
  _ <- M.string "nearby tickets:\n"
  otherTickets <- pTicket `P.sepBy1NG` M.newline
  return (rules, yourTicket, otherTickets)

parseInput :: String -> ( [Rule]   -- all rules
                        , Ticket   -- your ticket
                        , [Ticket] -- other tickets
                        )
parseInput = parseOrError pInput


-- Part 1 --

matchesRule :: Rule -> Int -> Bool
matchesRule (Rule _ ints) x = x `IS.member` ints

matchesAnyRule :: [Rule] -> Int -> Bool
matchesAnyRule rules x = any (`matchesRule` x) rules

-- | >>> part1 <$> readFile "input/2020/day16.txt"
-- 29759
part1 :: String -> Int
part1 input = sum $ filter isInvalid (concat otherTickets)
  where (rules, _, otherTickets) = parseInput input
        isInvalid = not . matchesAnyRule rules


{- Part 2

Now that you've identified which tickets contain invalid values, discard those
tickets entirely. Use the remaining valid tickets to determine which field is
which.

Using the valid ranges for each field, determine what order the fields appear
on the tickets. The order is consistent between all tickets: if seat is the
third field, it is the third field on every ticket, including your ticket.

For example, suppose you have the following notes:

    class: 0-1 or 4-19
    row: 0-5 or 8-19
    seat: 0-13 or 16-19

    your ticket:
    11,12,13

    nearby tickets:
    3,9,18
    15,1,5
    5,14,9

Based on the nearby tickets in the above example, the first position must be
row, the second position must be class, and the third position must be seat;
you can conclude that in your ticket, class is 12, row is 11, and seat is 13.

Once you work out which field is which, look for the six fields on your ticket
that start with the word departure. What do you get if you multiply those six
                                                       values together?

-}

-- | Determines which rule belongs to which field
matchRulesToFields :: [Rule] -> [Ticket] -> Map Int Rule
matchRulesToFields rules tickets =
  let candidates = fixedPoint refineCandidateList $ makeCandidateList rules tickets
  in Map.map (Set.elemAt 0) candidates

-- | Runs one candidiate "refinement" pass: finds any fields that only have 1
-- candidate rule, and removes those rules as candidates for other fields.
refineCandidateList :: Map Int (Set Rule) -> Map Int (Set Rule)
refineCandidateList candidates = Map.map removeSingles candidates
  where singles = Set.unions . filter ((== 1) . Set.size) . Map.elems $ candidates
        removeSingles oldSet
          | Set.size oldSet > 1  = Set.difference oldSet singles
          | otherwise            = oldSet

-- | Builds the initial candidate list. Rules are candidates for a field when
-- the rule works for all tickets.
makeCandidateList :: [Rule] -> [Ticket] -> Map Int (Set Rule)
makeCandidateList rules tickets =
  let fields = transpose tickets
      isCandidate fieldVals rule = all (matchesRule rule) fieldVals
      candidates = [Set.fromList $ filter (isCandidate fs) rules | fs <- fields]
  in Map.fromList $ zip [0..] candidates

-- | >>> part2 <$> readFile "input/2020/day16.txt"
-- 1307550234719
part2 :: String -> Int
part2 input = product $ map (yourTicket !!) departureFields
  where
    (rules, yourTicket, otherTickets) = parseInput input
    validTickets = filter isValid otherTickets
    ruleFieldMap = matchRulesToFields rules validTickets
    departureFields = Map.keys $ Map.filter (startsWith "departure" . ruleName) ruleFieldMap
    isValid = all (matchesAnyRule rules)
    ruleName (Rule name _) = name
    startsWith substr str = substr == take (length substr) str
