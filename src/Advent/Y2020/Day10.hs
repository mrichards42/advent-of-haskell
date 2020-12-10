{- Adapter Array -}
module Advent.Y2020.Day10 (part1, part2) where

import Data.List (sort, foldl', subsequences, group)
import Data.Map (Map)
import qualified Data.Map as Map

{- Part 1

Patched into the aircraft's data port, you discover weather forecasts of a
massive tropical storm. Before you can figure out whether it will impact your
vacation plans, however, your device suddenly turns off!

Its battery is dead.

You'll need to plug it in. There's only one problem: the charging outlet near
your seat produces the wrong number of jolts. Always prepared, you make a list
of all of the joltage adapters in your bag.

Each of your joltage adapters is rated for a specific output joltage (your
puzzle input). Any given adapter can take an input 1, 2, or 3 jolts lower than
its rating and still produce its rated output joltage.

In addition, your device has a built-in joltage adapter rated for 3 jolts
higher than the highest-rated adapter in your bag. (If your adapter list were
3, 9, and 6, your device's built-in adapter would be rated for 12 jolts.)

Treat the charging outlet near your seat as having an effective joltage rating
of 0.

Since you have some time to kill, you might as well test all of your adapters.
Wouldn't want to get to your resort and realize you can't even charge your
device!

If you use every adapter in your bag at once, what is the distribution of
joltage differences between the charging outlet, the adapters, and your device?

For example, suppose that in your bag, you have adapters with the following
joltage ratings:

  16 10 15 5 1 11 7 19 6 12 4

With these adapters, your device's built-in joltage adapter would be rated for
19 + 3 = 22 jolts, 3 higher than the highest-rated adapter.

Because adapters can only connect to a source 1-3 jolts lower than its rating,
in order to use every adapter, you'd need to choose them like this:

  - The charging outlet has an effective rating of 0 jolts, so the only
    adapters that could connect to it directly would need to have a joltage
    rating of 1, 2, or 3 jolts. Of these, only one you have is an adapter rated
    1 jolt (difference of 1).
  - From your 1-jolt rated adapter, the only choice is your 4-jolt rated
    adapter (difference of 3).
  - From the 4-jolt rated adapter, the adapters rated 5, 6, or 7 are valid
    choices. However, in order to not skip any adapters, you have to pick the
    adapter rated 5 jolts (difference of 1).
  - Similarly, the next choices would need to be the adapter rated 6 and then
    the adapter rated 7 (with difference of 1 and 1).
  - The only adapter that works with the 7-jolt rated adapter is the one rated
    10 jolts (difference of 3).
  - From 10, the choices are 11 or 12; choose 11 (difference of 1) and then 12
    (difference of 1).
  - After 12, only valid adapter has a rating of 15 (difference of 3), then 16
    (difference of 1), then 19 (difference of 3).
  - Finally, your device's built-in adapter is always 3 higher than the highest
    adapter, so its rating is 22 jolts (always a difference of 3).

In this example, when using every adapter, there are 7 differences of 1 jolt
and 5 differences of 3 jolts.

Here is a larger example:

  28 33 18 42 31 14 46 20 48 47 24 23 49 45 19
  38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3

In this larger example, in a chain that uses all of the adapters, there are 22
differences of 1 jolt and 10 differences of 3 jolts.

Find a chain that uses all of your adapters to connect the charging outlet to
your device's built-in adapter and count the joltage differences between the
charging outlet, the adapters, and your device. What is the number of 1-jolt
differences multiplied by the number of 3-jolt differences?

-}

parseInput :: String -> [Int]
parseInput = addEnds . sort . map read . lines
  where addEnds xs = [0] ++ xs ++ [maximum xs + 3]

diffs :: [Int] -> [Int]
diffs [] = []
diffs [_] = []
diffs (a:b:cs) = b - a : diffs (b:cs)

freqs :: Ord a => [a] -> Map a Int
freqs = foldl' accum Map.empty
  where accum m x = Map.alter update x m
        update old = case old of
                       Nothing -> Just 1
                       Just n -> Just $ n + 1

solve1 :: [Int] -> Int
solve1 xs = let j = freqs $ diffs xs
         in j Map.! 1 * j Map.! 3

-- | >>> part1 "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"
-- 35
-- >>> part1 "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"
-- 220
-- >>> part1 <$> readFile "input/2020/day10.txt"
-- 2380
part1 :: String -> Int
part1 = solve1 . parseInput


{- Part 2

To completely determine whether you have enough adapters, you'll need to figure
out how many different ways they can be arranged. Every arrangement needs to
connect the charging outlet to your device. The previous rules about when
adapters can successfully connect still apply.

The first example above (the one that starts with 16, 10, 15) supports the
following arrangements:

  (0) 1 4 5 6 7 10 11 12 15 16 19 (22)
  (0) 1 4 5 6 7 10 12 15 16 19 (22)
  (0) 1 4 5 7 10 11 12 15 16 19 (22)
  (0) 1 4 5 7 10 12 15 16 19 (22)
  (0) 1 4 6 7 10 11 12 15 16 19 (22)
  (0) 1 4 6 7 10 12 15 16 19 (22)
  (0) 1 4 7 10 11 12 15 16 19 (22)
  (0) 1 4 7 10 12 15 16 19 (22)

(The charging outlet and your device's built-in adapter are shown in
parentheses.) Given the adapters from the first example, the total number of
arrangements that connect the charging outlet to your device is 8.

The second example above (the one that starts with 28, 33, 18) has many
arrangements. Here are a few:

  (0) 1 2 3 4 7 8 9 10 11 14 17 18 19 20 23 24 25 28 31 32 33 34 35 38 39 42 45 46 47 48 49 (52)

  (0) 1 2 3 4 7 8 9 10 11 14 17 18 19 20 23 24 25 28 31 32 33 34 35 38 39 42 45 46 47 49 (52)

  (0) 1 2 3 4 7 8 9 10 11 14 17 18 19 20 23 24 25 28 31 32 33 34 35 38 39 42 45 46 48 49 (52)

  (0) 1 2 3 4 7 8 9 10 11 14 17 18 19 20 23 24 25 28 31 32 33 34 35 38 39 42 45 46 49 (52)

  (0) 1 2 3 4 7 8 9 10 11 14 17 18 19 20 23 24 25 28 31 32 33 34 35 38 39 42 45 47 48 49 (52)

  (0) 3 4 7 10 11 14 17 20 23 25 28 31 34 35 38 39 42 45 46 48 49 (52)

  (0) 3 4 7 10 11 14 17 20 23 25 28 31 34 35 38 39 42 45 46 49 (52)

  (0) 3 4 7 10 11 14 17 20 23 25 28 31 34 35 38 39 42 45 47 48 49 (52)

  (0) 3 4 7 10 11 14 17 20 23 25 28 31 34 35 38 39 42 45 47 49 (52)

  (0) 3 4 7 10 11 14 17 20 23 25 28 31 34 35 38 39 42 45 48 49 (52)

In total, this set of adapters can connect the charging outlet to your device
in 19208 distinct arrangements.

You glance back down at your bag and try to remember why you brought so many
adapters; there must be more than a trillion valid ways to arrange them!
Surely, there must be an efficient way to count the arrangements.

What is the total number of distinct ways you can arrange the adapters to
connect the charging outlet to your device?

-}

-- vvvv -- Thinking about how to do this -- vvvv --

{-

Given that the data only ever seems to have jumps of 1 and 3 (no 2s) I believe
we can basically ignore all the 3s and focus on groups of 1?

e.q.

  (0) 1 2 5 6 7 (10)

has 2 groups of 1s: [0 1 2] and [5 6 7]

the edges of each group have to stay, i.e

  2 <-> 5 can't be replaced, and
  7 <-> 10 can't be replaced

so the options are

  [[0, 1, 2], [0, 2]] + [[5, 6, 7], [5, 7]] + [[10]]

Meaning we need to figure out the number of ways to make each run of singles:

  2 * 2 * 1 = 4

Number of ways runs of N can be formed

  1   1  [1]
  2   1  [1 2]
  3   2  [1 2 3] [1 3]
  4   4  [1 2 3 4] [1 2 4] [1 3 4] [1 4]
  5   7  [1 4 5] [1 2 4 5] [1 3 4 5] [1 2 3 4 5]
         [1 3 4] [1 2 3 5]
         [1 2 5]


So for the first example:

  (0) 1 4 5 6 7 10 11 12 15 16 19 (22)

breaks down into

  [0, 1] ++ [4, 5, 6, 7] + [10, 11, 12] ++ [15, 16] ++ [19] ++ [22]

   1     *   4           *  2            *  1        *  1    *  1
     = 8

-}

-- This is pathological in large cases, but it looks like our input only has
-- short runs, so this is totally reasonable
-- Turns out it's also the "tribonacci" sequence: f (n-1) + f (n-2) + f (n-3)
listTheWays :: [Int] -> [[Int]]
listTheWays xs
  | length xs <= 2 = [xs]
  | otherwise = let a = head xs
                    b = last xs
                    mid = init $ tail xs
                in filter valid [[a] ++ m ++ [b] | m <- subsequences mid]
                where valid xs' = maximum (diffs xs') <= 3

countTheWays :: Int -> Int
countTheWays groupLength = length $ listTheWays [1..groupLength]

solve2 :: [Int] -> Int
solve2 = product . map groupWays . group . diffs
  where groupWays :: [Int] -> Int
        groupWays xs@(1:_) = countTheWays (length xs + 1)
        groupWays (3:_) = 1
        groupWays _ = 1

-- | >>> part2 "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"
-- 8
-- | >>> part2 "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"
-- 19208
-- | >>> part2 <$> readFile "input/2020/day10.txt"
-- 48358655787008
part2 :: String -> Int
part2 = solve2 . parseInput
