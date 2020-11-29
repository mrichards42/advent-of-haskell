{- Secure Container -}
module Advent.Y2019.Day04 (part1, part2) where

import Data.List (group, nub, sort)

{- Part 1

You arrive at the Venus fuel depot only to discover it's protected by a
password. The Elves had written the password on a sticky note, but someone
threw it out.

However, they do remember a few key facts about the password:

  - It is a six-digit number.
  - The value is within the range given in your puzzle input.
  - Two adjacent digits are the same (like 22 in 122345).
  - Going from left to right, the digits never decrease; they only ever
    increase or stay the same (like 111123 or 135679).

Other than the range rule, the following are true:

  - 111111 meets these criteria (double 11, never decreases).
  - 223450 does not meet these criteria (decreasing pair of digits 50).
  - 123789 does not meet these criteria (no double).

How many different passwords within the range given in your puzzle input meet
these criteria?

-}

parseInput :: String -> (Int, Int)
parseInput input =
  let [a,b] = lines input
  in (read a, read b)

toDigitList :: Int -> [Int]
toDigitList x = case x `divMod` 10 of
                  (0, r) -> [r]
                  (d, r) -> toDigitList d ++ [r]

-- | Produces a list of digits of candidate numbers.
-- Handles the "between a and b", and "in sorted order" criteria.
-- This is _way_ faster than the naive method of testing each number, since
-- almost all of them will fail the "sorted" criterion, and this method skips
-- generating unsorted numbers.
-- Compare the search space:
--   length [100000..999999]                  => 900000
--   length $ candidateNumbers 100000 999999  =>   3003
candidateNumbers :: Int -> Int -> [[Int]]
candidateNumbers a b = filter (\n -> (n >= a') && (n <= b')) ns
  where
    a' = toDigitList a
    b' = toDigitList b
    ns = [[n1, n2, n3, n4, n5, n6] | n1 <- [head a' .. head b' + 1]
                                   , n2 <- [n1..9]
                                   , n3 <- [n2..9]
                                   , n4 <- [n3..9]
                                   , n5 <- [n4..9]
                                   , n6 <- [n5..9]]


-- | >>> readFile "input/2019/day04.txt" >>= return . part1
-- 1929
part1 :: String -> Int
part1 = length . filter isCandidate . uncurry candidateNumbers . parseInput
  where isCandidate digits = nub digits /= digits

{- Part 2

An Elf just remembered one more important detail: the two adjacent matching
digits are not part of a larger group of matching digits.

Given this additional criterion, but still ignoring the range rule, the
following are now true:

  - 112233 meets these criteria because the digits never decrease and all
    repeated digits are exactly two digits long.
  - 123444 no longer meets the criteria (the repeated 44 is part of a larger
    group of 444).
  - 111122 meets the criteria (even though 1 is repeated more than twice, it
    still contains a double 22).

How many different passwords within the range given in your puzzle input meet
all of the criteria?


-}

-- | >>> readFile "input/2019/day04.txt" >>= return . part2
-- 1306
part2 :: String -> Int
part2 = length . filter isCandidate . uncurry candidateNumbers . parseInput
  where isCandidate = any ((== 2) . length) . group
