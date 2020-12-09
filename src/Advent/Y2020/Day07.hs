{- Handy Haversacks -}
module Advent.Y2020.Day07 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import qualified Advent.Util.Parsing as P
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as Graph
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

{- Part 1

You land at the regional airport in time for your next flight. In fact, it
looks like you'll even have time to grab some food: all flights are currently
delayed due to issues in luggage processing.

Due to recent aviation regulations, many rules (your puzzle input) are being
enforced about bags and their contents; bags must be color-coded and must
contain specific quantities of other color-coded bags. Apparently, nobody
responsible for these regulations considered how long they would take to
enforce!

For example, consider the following rules:

  light red bags contain 1 bright white bag, 2 muted yellow bags.
  dark orange bags contain 3 bright white bags, 4 muted yellow bags.
  bright white bags contain 1 shiny gold bag.
  muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
  shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
  dark olive bags contain 3 faded blue bags, 4 dotted black bags.
  vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
  faded blue bags contain no other bags.
  dotted black bags contain no other bags.

These rules specify the required contents for 9 bag types. In this example,
every faded blue bag is empty, every vibrant plum bag contains 11 bags (5 faded
blue and 6 dotted black), and so on.

You have a shiny gold bag. If you wanted to carry it in at least one other bag,
how many different bag colors would be valid for the outermost bag? (In other
words: how many colors can, eventually, contain at least one shiny gold bag?)

In the above rules, the following options would be available to you:

  - A bright white bag, which can hold your shiny gold bag directly.
  - A muted yellow bag, which can hold your shiny gold bag directly, plus some
    other bags.
  - A dark orange bag, which can hold bright white and muted yellow bags,
    either of which could then hold your shiny gold bag.
  - A light red bag, which can hold bright white and muted yellow bags, either
    of which could then hold your shiny gold bag.

So, in this example, the number of bag colors that can eventually contain at
least one shiny gold bag is 4.

How many bag colors can eventually contain at least one shiny gold bag? (The
list of rules is quite long; make sure you get all of it.)

-}

-- Naming is hard.
data Bag = Bag { bagAdjective :: String
               , bagColor :: String
               }
               deriving (Show, Eq, Ord)

type Rule = Map Bag Int
type AllRules = Map Bag Rule

-- | >>> parseOrError pBag "light red bags"
-- Bag "light" "red"
-- | >>> parseOrError pBag "bright white bag"
-- Bag "bright" "white"
pBag :: Parser Bag
pBag = do
  adj   <- M.some M.alphaNumChar <* M.space
  color <- M.some M.alphaNumChar <* M.space
  _ <- M.string "bag" <* M.optional (M.char 's')
  return $ Bag adj color

-- | >>> parseOrError pBagRule "light red bags contain 1 bright white bag, 2 muted yellow bags."
-- Map.fromList [(Bag "light" "red", Map.fromList [(Bag "bright" "white", 1), (Bag "muted" "yellow", 2)])]
pBagRule :: Parser AllRules
pBagRule = do
  container <- pBag
  _ <- M.string " contain "
  rule <- pRule
  return $ Map.fromList [(container, rule)]
  where pRule :: Parser Rule
        pRule = M.choice [ M.try pRule'
                         , Map.empty <$ M.string "no other bags."
                         ]
        pRule' = Map.unions <$> (pRule1 `P.sepBy1NG` bagSep) <* bagEnd
        bagSep = M.char ',' <* M.space
        bagEnd = M.char '.'

        pRule1 :: Parser Rule
        pRule1 = do
          count <- L.decimal
          _ <- M.space
          bag <- pBag
          return $ Map.fromList [(bag, count)]

parseInput :: String -> AllRules
parseInput = Map.unions . parseOrError (pBagRule `P.sepBy1NG` M.newline)

-- | Creates a Graph from a set of rules
-- This is a directed graph, from parent -> child
bagGraph :: AllRules -> (Graph, Vertex -> (Bag, Bag, [Bag]), Bag -> Maybe Vertex)
bagGraph = Graph.graphFromEdges . map toEdge . Map.toAscList
  where toEdge :: (Bag, Rule) -> (Bag, Bag, [Bag])
        toEdge (node, edgeMap) = (node, node, Map.keys edgeMap)

parentBags :: Bag -> AllRules -> Set Bag
parentBags child rules =
  maybe Set.empty reachableParents (getVertex child)
  where
    (g, getBagData, getVertex) = bagGraph rules
    getBag = fst3 . getBagData
    -- We want to traverse up through parents, so flip the graph.
    g' = Graph.transposeG g

    reachableParents :: Vertex -> Set Bag
    reachableParents c =
        Set.map getBag
      . Set.delete c -- don't include the starting node.
      . Set.fromList
      . Graph.reachable g'
      $ c

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | >>> parentBags (Bag "shiny" "gold") . parseInput $ "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.\n"
-- Set.fromList [Bag "bright" "white", Bag "muted" "yellow", Bag "dark" "orange", Bag "light" "red"]
-- | >>> part1 <$> readFile "input/2020/day07.txt"
-- 179
part1 :: String -> Int
part1 = Set.size . parentBags (Bag "shiny" "gold") . parseInput


{- Part 2

It's getting pretty expensive to fly these days - not because of ticket prices,
but because of the ridiculous number of bags you need to buy!

Consider again your shiny gold bag and the rules from the above example:

  - faded blue bags contain 0 other bags.
  - dotted black bags contain 0 other bags.
  - vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted
    black bags.
  - dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black
    bags.

So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags
within it) plus 2 vibrant plum bags (and the 11 bags within each of those): 1 +
1*7 + 2 + 2*11 = 32 bags!

Of course, the actual rules have a small chance of going several levels deeper
than this example; be sure to count all of the bags, even if the nesting
becomes topologically impractical!

Here's another example:

  shiny gold bags contain 2 dark red bags.
  dark red bags contain 2 dark orange bags.
  dark orange bags contain 2 dark yellow bags.
  dark yellow bags contain 2 dark green bags.
  dark green bags contain 2 dark blue bags.
  dark blue bags contain 2 dark violet bags.
  dark violet bags contain no other bags.

In this example, a single shiny gold bag must contain 126 other bags.

How many individual bags are required inside your single shiny gold bag?

-}

nestedBagCount :: Bag -> AllRules -> Int
nestedBagCount bag rules = sum . map childBagCount . Map.toAscList $ rules Map.! bag
  where childBagCount (b, n) = n + n * nestedBagCount b rules


-- | >>> part2 "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags.\n"
-- 126
-- >>> part2 <$> readFile "input/2020/day07.txt"
-- 18925
part2 :: String -> Int
part2 = nestedBagCount (Bag "shiny" "gold") . parseInput
