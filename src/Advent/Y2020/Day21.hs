{- Allergen Assessment -}
module Advent.Y2020.Day21 (part1, part2) where

import Advent.Util.Function (fixedPoint)
import Advent.Util.Parsing (Parser, parseOrError)
import qualified Advent.Util.Parsing as P
import Data.List (foldl', intercalate, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

{- Part 1

You reach the train's last stop and the closest you can get to your vacation
island without getting wet. There aren't even any boats here, but nothing can
stop you now: you build a raft. You just need a few days' worth of food for
your journey.

You don't speak the local language, so you can't read any ingredients lists.
However, sometimes, allergens are listed in a language you do understand. You
should be able to use this information to determine which ingredient contains
which allergen and work out which foods are safe to take with you on your trip.

You start by compiling a list of foods (your puzzle input), one food per line.
Each line includes that food's ingredients list followed by some or all of the
allergens the food contains.

Each allergen is found in exactly one ingredient. Each ingredient contains zero
or one allergen. Allergens aren't always marked; when they're listed (as in
(contains nuts, shellfish) after an ingredients list), the ingredient that
contains each listed allergen will be somewhere in the corresponding
ingredients list. However, even if an allergen isn't listed, the ingredient
that contains that allergen could still be present: maybe they forgot to label
it, or maybe it was labeled in a language you don't know.

For example, consider the following list of foods:

    mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
    trh fvjkl sbzzf mxmxvkd (contains dairy)
    sqjhc fvjkl (contains soy)
    sqjhc mxmxvkd sbzzf (contains fish)

The first food in the list has four ingredients (written in a language you
don't understand): mxmxvkd, kfcds, sqjhc, and nhms. While the food might
contain other allergens, a few allergens the food definitely contains are
listed afterward: dairy and fish.

The first step is to determine which ingredients can't possibly contain any of
the allergens in any food in your list. In the above example, none of the
ingredients kfcds, nhms, sbzzf, or trh can contain an allergen. Counting the
number of times any of these ingredients appear in any ingredients list
produces 5: they all appear once each except sbzzf, which appears twice.

Determine which ingredients cannot possibly contain any of the allergens in
your list. How many times do any of those ingredients appear?

-}


-- Parsing --

type Ingredient = String
type Allergen = String
type Ingredients = [Ingredient]
type Allergens = [Allergen]

pWord :: Parser String
pWord = M.some M.lowerChar

pIngredients :: Parser Ingredients
pIngredients = pWord `M.sepEndBy1` M.char ' '

pAllergens :: Parser Allergens
pAllergens = do
  _ <- M.string "(contains "
  allergens <- pWord `M.sepBy1` M.string ", "
  _ <- M.char ')'
  return allergens

pLine :: Parser (Ingredients, Allergens)
pLine = do
  ingredients <- pIngredients
  allergens <- M.choice [ pAllergens
                        , [] <$ M.eol
                        ]
  return (ingredients, allergens)

parseInput :: String -> [(Ingredients, Allergens)]
parseInput = parseOrError $ pLine `P.sepBy1NG` M.newline


-- Allergen Discovery --

type Freqs a = Map a Int
type AllergenCandidateMap = Map Allergen (Freqs Ingredient)

-- From day 10
freqs :: Ord a => [a] -> Freqs a
freqs = foldl' accum Map.empty
  where accum m x = Map.alter update x m
        update = Just . maybe 1 (+ 1)

allergenCandidates :: [(Ingredients, Allergens)] -> AllergenCandidateMap
allergenCandidates foods =
  Map.unionsWith (Map.unionWith (+))
  [ Map.singleton a (freqs is) | (is, as) <- foods , a <- as ]

findAllergens :: AllergenCandidateMap -> AllergenCandidateMap
findAllergens = fixedPoint step
  where step = removeSingles . Map.map filterMax

        filterMax :: Freqs a -> Freqs a
        filterMax freqMap =
          let maxFreq = maximum (Map.elems freqMap)
          in Map.filter (== maxFreq) freqMap

        removeSingles :: AllergenCandidateMap -> AllergenCandidateMap
        removeSingles m =
          let singles = Map.unions . filter isSingle $ Map.elems m
              isSingle = (== 1) . Map.size
          in Map.map (\v -> if isSingle v then v else v `Map.difference` singles) m


-- | >>> part1 "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)\n"
-- 5
-- >>> part1 <$> readFile "input/2020/day21.txt"
-- 1958
part1 :: String -> Int
part1 input =
  let foods = parseInput input
      allergensByIngredientCount = findAllergens $ allergenCandidates foods
      badIngredients = Map.unions . Map.elems $ allergensByIngredientCount
      allIngredients = concatMap fst foods
  in length . filter (not . (`Map.member` badIngredients)) $ allIngredients


{- Part 2

Now that you've isolated the inert ingredients, you should have enough
information to figure out which ingredient contains which allergen.

In the above example:

  - mxmxvkd contains dairy.
  - sqjhc contains fish.
  - fvjkl contains soy.

Arrange the ingredients alphabetically by their allergen and separate them by
commas to produce your canonical dangerous ingredient list. (There should not
be any spaces in your canonical dangerous ingredient list.) In the above
example, this would be mxmxvkd,sqjhc,fvjkl.

Time to stock your raft with supplies. What is your canonical dangerous
ingredient list?

-}

-- | >>> part2 "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)\n"
-- "mxmxvkd,sqjhc,fvjkl"
-- | >>> part2 <$> readFile "input/2020/day21.txt"
-- "xxscc,mjmqst,gzxnc,vvqj,trnnvn,gbcjqbm,dllbjr,nckqzsg"
part2 :: String -> String
part2 input =
  let foods = parseInput input
      allergensByIngredientCount = findAllergens $ allergenCandidates foods
      allergenIngredientPairs = Map.assocs . Map.map (head . Map.keys) $ allergensByIngredientCount
  in intercalate "," .map snd . sortOn fst $ allergenIngredientPairs
