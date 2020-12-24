{- Crab Cups -}
module Advent.Y2020.Day23 (part1, part2) where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

-- For part2
import Control.Monad.ST (ST)
import Data.List (sort)
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV

{- Part 1

The small crab challenges you to a game! The crab is going to mix up some cups,
and you have to predict where they'll end up.

The cups will be arranged in a circle and labeled clockwise (your puzzle
input). For example, if your labeling were 32415, there would be five cups in
the circle; going clockwise around the circle from the first cup, the cups
would be labeled 3, 2, 4, 1, 5, and then back to 3 again.

Before the crab starts, it will designate the first cup in your list as the
current cup. The crab is then going to do 100 moves.

Each move, the crab does the following actions:

  - The crab picks up the three cups that are immediately clockwise of the
    current cup. They are removed from the circle; cup spacing is adjusted as
    necessary to maintain the circle.
  - The crab selects a destination cup: the cup with a label equal to the
    current cup's label minus one. If this would select one of the cups that
    was just picked up, the crab will keep subtracting one until it finds a cup
    that wasn't just picked up. If at any point in this process the value goes
    below the lowest value on any cup's label, it wraps around to the highest
    value on any cup's label instead.
  - The crab places the cups it just picked up so that they are immediately
    clockwise of the destination cup. They keep the same order as when they
    were picked up.
  - The crab selects a new current cup: the cup which is immediately clockwise
    of the current cup.

For example, suppose your cup labeling were 389125467. If the crab were to do
merely 10 moves, the following changes would occur:

    -- move 1 --
    cups: (3) 8  9  1  2  5  4  6  7 
    pick up: 8, 9, 1
    destination: 2

    -- move 2 --
    cups:  3 (2) 8  9  1  5  4  6  7 
    pick up: 8, 9, 1
    destination: 7

    -- move 3 --
    cups:  3  2 (5) 4  6  7  8  9  1 
    pick up: 4, 6, 7
    destination: 3

    -- move 4 --
    cups:  7  2  5 (8) 9  1  3  4  6 
    pick up: 9, 1, 3
    destination: 7

    -- move 5 --
    cups:  3  2  5  8 (4) 6  7  9  1 
    pick up: 6, 7, 9
    destination: 3

    -- move 6 --
    cups:  9  2  5  8  4 (1) 3  6  7 
    pick up: 3, 6, 7
    destination: 9

    -- move 7 --
    cups:  7  2  5  8  4  1 (9) 3  6 
    pick up: 3, 6, 7
    destination: 8

    -- move 8 --
    cups:  8  3  6  7  4  1  9 (2) 5 
    pick up: 5, 8, 3
    destination: 1

    -- move 9 --
    cups:  7  4  1  5  8  3  9  2 (6)
    pick up: 7, 4, 1
    destination: 5

    -- move 10 --
    cups: (5) 7  4  1  8  3  9  2  6 
    pick up: 7, 4, 1
    destination: 3

    -- final --
    cups:  5 (8) 3  7  4  1  9  2  6 

In the above example, the cups' values are the labels as they appear moving
clockwise around the circle; the current cup is marked with ( ).

After the crab is done, what order will the cups be in? Starting after the cup
labeled 1, collect the other cups' labels clockwise into a single string with
no extra characters; each number except 1 should appear exactly once. In the
above example, after 10 moves, the cups clockwise from 1 are labeled 9, 2, 6,
5, and so on, producing 92658374. If the crab were to complete all 100 moves,
the order after cup 1 would be 67384529.

Using your labeling, simulate 100 moves. What are the labels on the cups after
cup 1?

-}

type Cups = [Int]

parseInput :: String -> Cups
parseInput = map readIntChar . head . lines
  where readIntChar :: Char -> Int
        readIntChar x = read [x]

countDownTo1 :: Int -> [Int]
countDownTo1 x = [x,x-1..1]

playRound1 :: Cups -> Cups
playRound1 cups@( current : a : b : c : xs ) =
  let target = head
             . filter (\n -> n /= a && n /= b && n /= c)
             $ countDownTo1 (current - 1) ++ countDownTo1 (length cups)
      (before, after) = span (/= target) xs
  in before ++ [target, a, b, c] ++ drop 1 after ++ [current]
playRound1 _ = error "Must be a list of at least 5 elements"

rotateTil :: Int -> Cups -> Cups
rotateTil n (x : xs)
  | n == x = x : xs
  | otherwise = rotateTil n (xs ++ [x])
rotateTil _ _ = error "Cups must not be empty"

-- | >>> part1 "389125467"
-- "67384529"
-- >>> part1 <$> readFile "input/2020/day23.txt"
-- "98752463"
part1 :: String -> String
part1 = concatMap show . drop 1 . rotateTil 1 . (!! 100) . iterate playRound1 . parseInput


{- Part 2

Due to what you can only assume is a mistranslation (you're not exactly fluent
in Crab), you are quite surprised when the crab starts arranging many cups in a
circle on your raft - one million (1000000) in total.

Your labeling is still correct for the first few cups; after that, the
remaining cups are just numbered in an increasing fashion starting from the
number after the highest number in your list and proceeding one by one until
one million is reached. (For example, if your labeling were 54321, the cups
would be numbered 5, 4, 3, 2, 1, and then start counting up from 6 until one
million is reached.) In this way, every number from one through one million is
used exactly once.

After discovering where you made the mistake in translating Crab Numbers, you
realize the small crab isn't going to do merely 100 moves; the crab is going to
do ten million (10000000) moves!

The crab is going to hide your stars - one each - under the two cups that will
end up immediately clockwise of cup 1. You can have them if you predict what
the labels on those cups will be when the crab is finished.

In the above example (389125467), this would be 934001 and then 159792;
multiplying these together produces 149245887792.

Determine which two cups will end up immediately clockwise of cup 1. What do
you get if you multiply their labels together?

-}

-- Circular linked list data structure --

-- I ended up writing this in C first, and this is more or less a 1:1
-- translation to Haskell.

{-

Requirements:
* Fast inserts and deletions (since we need to move items around)
* Fast lookup (since we need to know _where_ to move items)

Constraints:
* Contains every number [1..n], but only once

We need a linked list for the fast insert/delete, _and_ some kind of
associative data structure for fast lookups. The constraint that every number
must appear only once means we can use a (dense) Vector for the lookups, and
means that we can store each node's successor as the value in the vector, which
gets us a linked list.

In the end, using a mutable Vector gets this close to 11 seconds (running in
ghci), which is reasonable enough (although the C version takes > 1 second).

-}

-- Since the lowest number is 1, we'll use 0 to store the head pointer
type CupList = Vector Int
type MCupList s = MVector s Int -- mutable version for speed

-- | Constructs a CupList from a list of integers. Input list must contain
-- every number from [1.. length list] in any order (this constraint is not
-- checked).
cupsFromList :: [Int] -> CupList
cupsFromList (x:xs) =
  let assocs = (0, x) : zip (x:xs) (xs ++ [x])
  in V.fromList . map snd . sort $ assocs
cupsFromList _ = error "nope"

parseInput2 :: String -> CupList
parseInput2 = cupsFromList . addTil1Million . parseInput
  where addTil1Million xs = xs ++ [length xs + 1 .. 1000000]

-- | Plays a single round, mutating the input CupList
playRoundST :: MCupList s -> ST s ()
playRoundST cups = do
  -- Take the head and the next 3 values
  h <- MV.read cups 0
  a <- MV.read cups h
  b <- MV.read cups a
  c <- MV.read cups b
  -- Find the target, where we will move the 3 values
  let exclude = [a, b, c]
  let maxTarget = MV.length cups - 1
  let adjustTarget x = case (h - x) `mod` maxTarget of
                         0 -> maxTarget
                         other -> other
  let target
       | h - 1 `notElem` exclude = adjustTarget 1
       | h - 2 `notElem` exclude = adjustTarget 2
       | h - 3 `notElem` exclude = adjustTarget 3
       | otherwise               = adjustTarget 4
  -- Move 3 values after target: order is important!
  MV.write cups h =<< MV.read cups c       -- head -> c + 1   (remove a, b, c)
  MV.write cups c =<< MV.read cups target  -- c -> target + 1 (insert c)
  MV.write cups target a                   -- target -> a     (insert a)
  -- next turn (increment head)
  MV.write cups 0 =<< MV.read cups h
  return ()

playGameST :: Int -> MCupList s -> ST s ()
playGameST n cups
  | n > 0 = playRoundST cups >> playGameST (n - 1) cups
  | otherwise = return ()

-- | >>> part2 <$> readFile "input/2020/day23.txt"
-- 2000455861
part2 :: String -> Int
part2 = result . V.modify (playGameST 10000000) . parseInput2
  where
    -- product of the first two values after `1`
    result cups = let a = cups V.! 1
                      b = cups V.! a
                  in a * b


-- Utils --

-- | Shows cups starting from `start`
_showCupsFrom :: Int -> CupList -> String
_showCupsFrom start cups = show start ++ go (cups V.! start)
  where
    go n
      | n == start = ""
      | otherwise  = " " ++ show n ++ go (cups V.! n)

-- | Shows cups starting from head
_showCups :: CupList -> String
_showCups cups = _showCupsFrom (cups V.! 0) cups
