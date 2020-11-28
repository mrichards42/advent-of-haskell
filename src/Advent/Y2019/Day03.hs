{- Crossed Wires -}
module Advent.Y2019.Day03 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import Data.List (foldl', foldl1')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

{- Part 1

The gravity assist was successful, and you're well on your way to the Venus
refuelling station. During the rush back on Earth, the fuel management system
wasn't completely installed, so that's next on the priority list.

Opening the front panel reveals a jumble of wires. Specifically, two wires
are connected to a central port and extend outward on a grid. You trace the
path each wire takes as it leaves the central port, one wire per line of text
(your puzzle input).

The wires twist and turn, but the two wires occasionally cross paths. To fix
the circuit, you need to find the intersection point closest to the central
port. Because the wires are on a grid, use the Manhattan distance for this
measurement. While the wires do technically cross right at the central port
where they both start, this point does not count, nor does a wire count as
crossing with itself.

For example, if the first wire's path is R8,U5,L5,D3, then starting from the
central port (o), it goes right 8, up 5, left 5, and finally down 3:

...........
...........
...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........

Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down
4, and left 4:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........

These wires cross at two locations (marked X), but the lower-left one is
closer to the central port: its distance is 3 + 3 = 6.

Here are a few more examples:

  - R75,D30,R83,U83,L12,D49,R71,U7,L72
  - U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
  - R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
  - U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135

What is the Manhattan distance from the central port to the closest
intersection?

-}

-- Parsing --

data Direction = DRight
               | DLeft
               | DUp
               | DDown
               deriving (Show)

type Instruction = (Direction, Int)

type WireInstructions = [Instruction]

pDirection :: Parser Instruction
pDirection =
  (,) DRight <$> (M.char 'R' *> L.decimal) <|>
  (,) DLeft  <$> (M.char 'L' *> L.decimal) <|>
  (,) DUp    <$> (M.char 'U' *> L.decimal) <|>
  (,) DDown  <$> (M.char 'D' *> L.decimal)

pWire :: Parser WireInstructions
pWire = pDirection `M.sepBy` M.char ','

parseInput :: String -> [WireInstructions]
parseInput = filter (not . null) . parseOrError (pWire `M.sepBy` M.newline)


-- Laying out wires --

-- Part 1 just requires knowing where wires cross, so this could be done with a
-- Set, but part 2 requires knowing how many steps it took to get to the
-- crossing, so we need a Map of point -> steps.

type Point = (Int, Int)
type StepCount = Int
type WirePoints = Map Point StepCount

layWireSegment :: (WirePoints, Point, StepCount)
               -> Instruction
               -> (WirePoints, Point, StepCount)
layWireSegment (w, startPt, stepCount) (dir, n) =
  (Map.union w segment, last points, stepCount + n)
  where
    points = take n $ drop 1 $ iterate step startPt
    segment = Map.fromList $ zip points [stepCount+1..]
    step :: Point -> Point
    step = case dir of
             DRight -> \(x, y) -> (x + 1, y    )
             DLeft  -> \(x, y) -> (x - 1, y    )
             DUp    -> \(x, y) -> (x,     y - 1)
             DDown  -> \(x, y) -> (x,     y + 1)

layWire :: WireInstructions -> WirePoints
layWire instructions =
  let (m, _, _) = foldl' layWireSegment (Map.empty, (0,0), 0) instructions
  in m

distance :: Point -> Point -> Int
distance (x, y) (x', y') = abs (x' - x) + abs (y' - y)

crossings :: [WirePoints] -> Set Point
crossings = foldl1' Set.intersection . map Map.keysSet


-- | test cases
-- >> part1 "R8,U5,L5,D3\nU7,R6,D4,L4"
-- 6
-- >>> part1 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
-- 159
-- >> part1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
-- 135

-- | >>> readFile "input/2019/day03.txt" >>= return . part1
-- 375
part1 :: String -> Int
part1 = Set.findMin
      . Set.map (distance (0,0))
      . crossings
      . map layWire
      . parseInput


{- Part 2

It turns out that this circuit is very timing-sensitive; you actually need to
minimize the signal delay.

To do this, calculate the number of steps each wire takes to reach each
intersection; choose the intersection where the sum of both wires' steps is
lowest. If a wire visits a position on the grid multiple times, use the steps
value from the first time it visits that position when calculating the total
value of a specific intersection.

The number of steps a wire takes is the total number of grid squares the wire
has entered to get to that location, including the intersection being
considered. Again consider the example from above:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........

In the above example, the intersection closest to the central port is reached
after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20 steps by the
second wire for a total of 20+20 = 40 steps.

However, the top-right intersection is better: the first wire takes only
8+5+2 = 15 and the second wire takes only 7+6+2 = 15, a total of 15+15 = 30
steps.

Here are the best steps for the extra examples from above:

  - R75,D30,R83,U83,L12,D49,R71,U7,L72
  - U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
  - R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
  - U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps

What is the fewest combined steps the wires must take to reach an
intersection?

-}

-- | test cases
-- >> part2 "R8,U5,L5,D3\nU7,R6,D4,L4"
-- 40
-- >>> part2 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
-- 610
-- >> part2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
-- 410

-- | >>> readFile "input/2019/day03.txt" >>= return . part2
-- 14746
part2 :: String -> Int
part2 input = Set.findMin $ Set.map totalSteps allCrossings
  where
    wires = map layWire $ parseInput input
    allCrossings = crossings wires
    totalSteps pt = sum $ map (Map.! pt) wires
