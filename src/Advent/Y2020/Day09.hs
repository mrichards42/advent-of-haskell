{- Title -}
module Advent.Y2020.Day09 (part1, part2) where

import Data.Maybe (fromJust)

{- Part 1

-}

parseInput :: String -> [Int]
parseInput = map read . lines

isValid :: Int -> [Int] -> Bool
isValid _ [] = False
isValid target (x:xs)
  | target - x == x      = isValid target xs -- skip duplicates
  | target - x `elem` xs = True
  | otherwise            = isValid target xs -- try the next number

-- | >>> validNth 5 [35, 20, 15, 25, 47, 40]
-- True
-- >>> validNth 5 [95, 102, 117, 150, 182, 127]
-- False
validNth :: Int -> [Int] -> Bool
validNth n xs =
  case splitAt n xs of
    (candidates, target:_) -> isValid target candidates
    _ -> False

-- | >>> firstInvalidNth 5 [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
-- Just 127
firstInvalidNth :: Int -> [Int] -> Maybe Int
firstInvalidNth n xs
  | n >= length xs = Nothing
  | validNth n xs  = firstInvalidNth n (tail xs)
  | otherwise      = Just $ xs !! n

solve1 :: Int -> [Int] -> Int
solve1 n = fromJust . firstInvalidNth n

-- | >>> part1 <$> readFile "input/2020/day09.txt"
-- 23278925
part1 :: String -> Int
part1 = solve1 25 . parseInput


{- Part 2

-}

-- | >>> findSumRun 127 [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
-- Just [15, 25, 47, 40]
findSumRun :: Int -> [Int] -> Maybe [Int]
findSumRun _ [] = Nothing
findSumRun target xs =
  case dropWhile (< target) $ scanl (+) 0 xs of
    [] -> Nothing
    test:_
      | test == target -> Just solution
      | otherwise      -> findSumRun target (tail xs)
  where
    solution = take (length $ takeWhile (< target) $ scanl (+) 0 xs) xs

-- | >>> solve2 5 [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
-- 62
solve2 :: Int -> [Int] -> Int
solve2 n xs =
  let target = solve1 n xs
      numbers = fromJust $ findSumRun target xs
      a = minimum numbers
      b = maximum numbers
  in a + b

-- | >>> part2 <$> readFile "input/2020/day09.txt"
-- 4011064
part2 :: String -> Int
part2 = solve2 25 . parseInput
