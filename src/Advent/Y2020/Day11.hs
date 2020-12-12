{- Seating System -}
module Advent.Y2020.Day11 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Map = HashMap

{- Part 1

Your plane lands with plenty of time to spare. The final leg of your journey is
a ferry that goes directly to the tropical island where you can finally start
your vacation. As you reach the waiting area to board the ferry, you realize
you're so early, nobody else has even arrived yet!

By modeling the process people use to choose (or abandon) their seat in the
waiting area, you're pretty sure you can predict the best place to sit. You
make a quick map of the seat layout (your puzzle input).

The seat layout fits neatly on a grid. Each position is either floor (.), an
empty seat (L), or an occupied seat (#). For example, the initial seat layout
might look like this:

  L.LL.LL.LL
  LLLLLLL.LL
  L.L.L..L..
  LLLL.LL.LL
  L.LL.LL.LL
  L.LLLLL.LL
  ..L.L.....
  LLLLLLLLLL
  L.LLLLLL.L
  L.LLLLL.LL

Now, you just need to model the people who will be arriving shortly.
Fortunately, people are entirely predictable and always follow a simple set of
rules. All decisions are based on the number of occupied seats adjacent to a
given seat (one of the eight positions immediately up, down, left, right, or
diagonal from the seat). The following rules are applied to every seat
simultaneously:

  - If a seat is empty (L) and there are no occupied seats adjacent to it, the
    seat becomes occupied.
  - If a seat is occupied (#) and four or more seats adjacent to it are also
    occupied, the seat becomes empty.
  - Otherwise, the seat's state does not change.

Floor (.) never changes; seats don't move, and nobody sits on the floor.

After one round of these rules, every seat in the example layout becomes
occupied:

  #.##.##.##
  #######.##
  #.#.#..#..
  ####.##.##
  #.##.##.##
  #.#####.##
  ..#.#.....
  ##########
  #.######.#
  #.#####.##

After a second round, the seats with four or more occupied adjacent seats
become empty again:

  #.LL.L#.##
  #LLLLLL.L#
  L.L.L..L..
  #LLL.LL.L#
  #.LL.LL.LL
  #.LLLL#.##
  ..L.L.....
  #LLLLLLLL#
  #.LLLLLL.L
  #.#LLLL.##

This process continues for three more rounds:

  #.##.L#.##
  #L###LL.L#
  L.#.#..#..
  #L##.##.L#
  #.##.LL.LL
  #.###L#.##
  ..#.#.....
  #L######L#
  #.LL###L.L
  #.#L###.##

  #.#L.L#.##
  #LLL#LL.L#
  L.L.L..#..
  #LLL.##.L#
  #.LL.LL.LL
  #.LL#L#.##
  ..L.L.....
  #L#LLLL#L#
  #.LLLLLL.L
  #.#L#L#.##

  #.#L.L#.##
  #LLL#LL.L#
  L.#.L..#..
  #L##.##.L#
  #.#L.LL.LL
  #.#L#L#.##
  ..L.L.....
  #L#L##L#L#
  #.LLLLLL.L
  #.#L#L#.##

At this point, something interesting happens: the chaos stabilizes and further
applications of these rules cause no seats to change state! Once people stop
moving around, you count 37 occupied seats.

Simulate your seating area by applying the seating rules repeatedly until no
seats change state. How many seats end up occupied?

-}

-- Parsing --
-- TODO: think about extracting this into a Grid module

data Seat = Occupied | Unoccupied | Floor
  deriving (Eq)
type Point = (Int, Int)
type Grid a = Map Point a
type SeatGrid = Grid Seat

pSeat :: Parser Seat
pSeat = 
  M.choice [ Occupied   <$ M.char '#'
           , Unoccupied <$ M.char 'L'
           , Floor      <$ M.char '.'
           ]

toGrid :: [[a]] -> Grid a
toGrid = Map.fromList . concat . mkgrid
  where
    mkgrid :: [[a]] -> [[(Point, a)]]
    mkgrid rows = zipWith mkrow [0..] rows

    mkrow :: Int -> [a] -> [(Point, a)]
    mkrow r xs = zipWith (mkelem r) [0..] xs

    mkelem :: Int -> Int -> a -> (Point, a)
    mkelem r c x = ((c, r), x)

pGrid :: Parser SeatGrid
pGrid = toGrid <$> M.some pSeat `M.sepEndBy1` M.newline

parseInput :: String -> SeatGrid
parseInput = parseOrError pGrid


-- Common machinery --

-- | Finds a fixed point of function `f` for input `x`, i.e. iterates until the
-- input equals the output.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x =
  let x' = f x
  in if x == x'
       then x'
       else fixedPoint f x'

-- | Memoizes a "neighbors" function (Grid -> Point -> neighbors) and a Grid,
-- returning a new function of just (Point -> neighbors)
neighborsMemo :: (Grid a -> Point -> [Point]) -> Grid a -> (Point -> [Point])
neighborsMemo neighbors grid =
  let neighborMap = Map.mapWithKey (\k _ -> neighbors grid k) grid
  -- Return a lambda so that `neighborMap` is part of the closure. Otherwise
  -- this seems to evaluate `neighbors` each time (which is the expensive part)
  in \pt -> Map.findWithDefault [] pt neighborMap

-- | Returns the number of neighbor seats that are currently Occupied
neighborCount :: (Point -> [Point]) -> SeatGrid -> Point -> Int
neighborCount neighbors g pt =
  length $ filter (== Occupied) $ lookups g (neighbors pt)
  where lookups m = mapMaybe (m Map.!?)

-- | Transforms a grid using the given rule function
gridStep :: (Grid a -> Point -> a -> a) -> Grid a -> Grid a
gridStep ruleFn grid = Map.mapWithKey (ruleFn grid) grid


-- Part 1 specific functions --

neighbors1 :: Grid a -> Point -> [Point]
neighbors1 g (x, y) =
  filter (`Map.member` g) [ (x + dx, y + dy) | dx <- [-1..1]
                                             , dy <- [-1..1]
                                             , (dx, dy) /= (0, 0) ]

seatRule1 :: (Point -> [Point]) -> SeatGrid -> Point -> Seat -> Seat
seatRule1 _ _ _ Floor = Floor
seatRule1 neighbors g pt seat
  | seat == Occupied   && occupiedNeighbors >= 4 = Unoccupied
  | seat == Unoccupied && occupiedNeighbors == 0 = Occupied
  | otherwise                                    = seat
  where occupiedNeighbors = neighborCount neighbors g pt

-- | >>> part1 "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
-- 37
-- >>> part1 <$> readFile "input/2020/day11.txt"
-- 2441
part1 :: String -> Int
part1 input =
  let seats = parseInput input
      rule = seatRule1 $ neighborsMemo neighbors1 seats
  in length
   $ filter (== Occupied)
   $ Map.elems
   $ fixedPoint (gridStep rule) seats


{- Part 2

As soon as people start to arrive, you realize your mistake. People don't just
care about adjacent seats - they care about the first seat they can see in each
of those eight directions!

Now, instead of considering just the eight immediately adjacent seats, consider
the first seat in each of those eight directions. For example, the empty seat
below would see eight occupied seats:

.......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....

The leftmost empty seat below would only see one empty seat, but cannot see any
of the occupied ones:

.............
.L.L.#.#.#.#.
.............

The empty seat below would see no occupied seats:

  .##.##.
  #.#.#.#
  ##...##
  ...L...
  ##...##
  #.#.#.#
  .##.##.

Also, people seem to be more tolerant than you expected: it now takes five or
more visible occupied seats for an occupied seat to become empty (rather than
four or more from the previous rules). The other rules still apply: empty seats
that see no occupied seats become occupied, seats matching no rule don't
change, and floor never changes.

Given the same starting layout as above, these new rules cause the seating area
to shift around as follows:

  L.LL.LL.LL
  LLLLLLL.LL
  L.L.L..L..
  LLLL.LL.LL
  L.LL.LL.LL
  L.LLLLL.LL
  ..L.L.....
  LLLLLLLLLL
  L.LLLLLL.L
  L.LLLLL.LL

  #.##.##.##
  #######.##
  #.#.#..#..
  ####.##.##
  #.##.##.##
  #.#####.##
  ..#.#.....
  ##########
  #.######.#
  #.#####.##

  #.LL.LL.L#
  #LLLLLL.LL
  L.L.L..L..
  LLLL.LL.LL
  L.LL.LL.LL
  L.LLLLL.LL
  ..L.L.....
  LLLLLLLLL#
  #.LLLLLL.L
  #.LLLLL.L#

  #.L#.##.L#
  #L#####.LL
  L.#.#..#..
  ##L#.##.##
  #.##.#L.##
  #.#####.#L
  ..#.#.....
  LLL####LL#
  #.L#####.L
  #.L####.L#

  #.L#.L#.L#
  #LLLLLL.LL
  L.L.L..#..
  ##LL.LL.L#
  L.LL.LL.L#
  #.LLLLL.LL
  ..L.L.....
  LLLLLLLLL#
  #.LLLLL#.L
  #.L#LL#.L#

  #.L#.L#.L#
  #LLLLLL.LL
  L.L.L..#..
  ##L#.#L.L#
  L.L#.#L.L#
  #.L####.LL
  ..#.#.....
  LLL###LLL#
  #.LLLLL#.L
  #.L#LL#.L#

  #.L#.L#.L#
  #LLLLLL.LL
  L.L.L..#..
  ##L#.#L.L#
  L.L#.LL.L#
  #.LLLL#.LL
  ..#.L.....
  LLL###LLL#
  #.LLLLL#.L
  #.L#LL#.L#

Again, at this point, people stop shifting around and the seating area reaches
equilibrium. Once this occurs, you count 26 occupied seats.

Given the new visibility method and the rule change for occupied seats becoming
empty, once equilibrium is reached, how many seats end up occupied?

-}

neighbors2 :: SeatGrid -> Point -> [Point]
neighbors2 g pt =
  catMaybes [ nextPt pt (dx, dy) | dx <- [-1..1]
                                 , dy <- [-1..1]
                                 , (dx, dy) /= (0, 0) ]
  where nextPt :: Point -> Point -> Maybe Point
        nextPt (x, y) (dx, dy) =
          let pt' = (x + dx, y + dy)
          in case g Map.!? pt' of
               Nothing    -> Nothing
               Just Floor -> nextPt pt' (dx, dy)
               _          -> Just pt'


seatRule2 :: (Point -> [Point]) -> SeatGrid -> Point -> Seat -> Seat
seatRule2 _ _ _ Floor = Floor
seatRule2 neighbors g pt seat
  | seat == Occupied   && occupiedNeighbors >= 5 = Unoccupied
  | seat == Unoccupied && occupiedNeighbors == 0 = Occupied
  | otherwise                                    = seat
  where occupiedNeighbors = neighborCount neighbors g pt

-- | >>> part2 "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
-- 26
-- >>> part2 <$> readFile "input/2020/day11.txt"
-- 2190
part2 :: String -> Int
part2 input =
  let seats = parseInput input
      rule = seatRule2 $ neighborsMemo neighbors2 seats
  in length
   $ filter (== Occupied)
   $ Map.elems
   $ fixedPoint (gridStep rule) seats
