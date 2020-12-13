{- Rain Risk -}
module Advent.Y2020.Day12 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import Data.List (foldl')
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

{- Part 1

Your ferry made decent progress toward the island, but the storm came in faster
than anyone expected. The ferry needs to take evasive actions!

Unfortunately, the ship's navigation computer seems to be malfunctioning;
rather than giving a route directly to safety, it produced extremely circuitous
instructions. When the captain uses the PA system to ask if anyone can help,
you quickly volunteer.

The navigation instructions (your puzzle input) consists of a sequence of
single-character actions paired with integer input values. After staring at
them for a few minutes, you work out what they probably mean:

  - Action N means to move north by the given value.
  - Action S means to move south by the given value.
  - Action E means to move east by the given value.
  - Action W means to move west by the given value.
  - Action L means to turn left the given number of degrees.
  - Action R means to turn right the given number of degrees.
  - Action F means to move forward by the given value in the direction the ship
    is currently facing.

The ship starts by facing east. Only the L and R actions change the direction
the ship is facing. (That is, if the ship is facing east and the next
instruction is N10, the ship would move north 10 units, but would still move
east if the following action were F.)

For example:

    F10
    N3
    F7
    R90
    F11

These instructions would be handled as follows:

  - F10 would move the ship 10 units east (because the ship starts by facing
    east) to east 10, north 0.
  - N3 would move the ship 3 units north to east 10, north 3.
  - F7 would move the ship another 7 units east (because the ship is still
    facing east) to east 17, north 3.
  - R90 would cause the ship to turn right by 90 degrees and face south; it
    remains at east 17, north 3.
  - F11 would move the ship 11 units south to east 17, south 8.

At the end of these instructions, the ship's Manhattan distance (sum of the
absolute values of its east/west position and its north/south position) from
its starting position is 17 + 8 = 25.

Figure out where the navigation instructions lead. What is the Manhattan
distance between that location and the ship's starting position?

-}


-- Data types and helpers --

type Point = (Int, Int)

data Angle = D0 | D90 | D180 | D270
    deriving (Eq, Show, Bounded, Enum)

data Instruction = North Int
                 | South Int
                 | East Int
                 | West Int
                 | TurnLeft Angle
                 | TurnRight Angle
                 | Forward Int
                 deriving (Eq, Show)

-- | Like `succ`, but wraps around
succ' :: (Eq a, Bounded a, Enum a) => a -> a
succ' x = if x == maxBound then minBound else succ x

angleFromInt :: Int -> Angle
angleFromInt   0 =   D0
angleFromInt  90 =  D90
angleFromInt 180 = D180
angleFromInt 270 = D270
angleFromInt x
  | x >= 360 || x < 0 = angleFromInt (x `mod` 360)
  | otherwise = error "Angle must be a multiple of 90"

invertAngle :: Angle -> Angle
invertAngle   D0 =   D0
invertAngle  D90 = D270
invertAngle D180 = D180
invertAngle D270 =  D90

rotateAngle :: Angle -- amount to rotate
            -> Angle -- original angle
            -> Angle
rotateAngle   D0 = id
rotateAngle  D90 = succ'
rotateAngle D180 = succ' . succ'
rotateAngle D270 = succ' . succ' . succ'


-- Parsing --

pAngle :: Parser Angle
pAngle = angleFromInt <$> L.decimal

pInstruction :: Parser Instruction
pInstruction = M.choice [ North     <$> (M.char 'N' *> L.decimal)
                        , South     <$> (M.char 'S' *> L.decimal)
                        , East      <$> (M.char 'E' *> L.decimal)
                        , West      <$> (M.char 'W' *> L.decimal)
                        , Forward   <$> (M.char 'F' *> L.decimal)
                        , TurnLeft  <$> (M.char 'L' *> pAngle)
                        , TurnRight <$> (M.char 'R' *> pAngle)
                        ]

parseInput :: String -> [Instruction]
parseInput = parseOrError (pInstruction `M.sepEndBy1` M.newline)


-- Solving part 1 --

type Heading = Angle

step1 :: Instruction -> (Point, Heading) -> (Point, Heading)
step1 instr = case instr of
                (North n)     -> fy (+ n)
                (South n)     -> fy (+ (-n))
                (East n)      -> fx (+ n)
                (West n)      -> fx (+ (- n))
                (TurnRight a) -> step1 (TurnLeft (invertAngle a))
                (TurnLeft a)  -> fh (rotateAngle a)
                (Forward n)   -> forward n
  where
    forward n (pt, heading) =
        case heading of
          D0   -> step1 (East n)  (pt, heading)
          D90  -> step1 (North n) (pt, heading)
          D180 -> step1 (West n)  (pt, heading)
          D270 -> step1 (South n) (pt, heading)
    -- sort of lenses?
    fx f ((x, y), heading) = ((f x,   y),   heading)
    fy f ((x, y), heading) = ((  x, f y),   heading)
    fh f ((x, y), heading) = ((  x,   y), f heading)

solve1 :: [Instruction] -> Point
solve1 = fst . foldl' (flip step1) ((0, 0), D0)

-- | >>> part1 "F10\nN3\nF7\nR90\nF11"
-- 25
-- >>> part1 <$> readFile "input/2020/day12.txt"
-- 820
part1 :: String -> Int
part1 input = let (x, y) = solve1 $ parseInput input
              in abs x + abs y


{- Part 2

Before you can give the destination to the captain, you realize that the actual
action meanings were printed on the back of the instructions the whole time.

Almost all of the actions indicate how to move a waypoint which is relative to
the ship's position:

  - Action N means to move the waypoint north by the given value.
  - Action S means to move the waypoint south by the given value.
  - Action E means to move the waypoint east by the given value.
  - Action W means to move the waypoint west by the given value.
  - Action L means to rotate the waypoint around the ship left
    (counter-clockwise) the given number of degrees.
  - Action R means to rotate the waypoint around the ship right (clockwise) the
    given number of degrees.
  - Action F means to move forward to the waypoint a number of times equal to
    the given value.

The waypoint starts 10 units east and 1 unit north relative to the ship. The
waypoint is relative to the ship; that is, if the ship moves, the waypoint
moves with it.

For example, using the same instructions as above:

  - F10 moves the ship to the waypoint 10 times (a total of 100 units east and
    10 units north), leaving the ship at east 100, north 10. The waypoint stays
    10 units east and 1 unit north of the ship.
  - N3 moves the waypoint 3 units north to 10 units east and 4 units north of
    the ship. The ship remains at east 100, north 10.
  - F7 moves the ship to the waypoint 7 times (a total of 70 units east and 28
    units north), leaving the ship at east 170, north 38. The waypoint stays 10
    units east and 4 units north of the ship.
  - R90 rotates the waypoint around the ship clockwise 90 degrees, moving it to
    4 units east and 10 units south of the ship. The ship remains at east 170,
    north 38.
  - F11 moves the ship to the waypoint 11 times (a total of 44 units east and
    110 units south), leaving the ship at east 214, south 72. The waypoint
    stays 4 units east and 10 units south of the ship.

After these operations, the ship's Manhattan distance from its starting
position is 214 + 72 = 286.

Figure out where the navigation instructions actually lead. What is the
Manhattan distance between that location and the ship's starting position?

-}

type Ship = Point
type Waypoint = Point

step2 :: Instruction -> (Ship, Waypoint) -> (Ship, Waypoint)
step2 instr = case instr of
                (North n)     -> waypointY (+ n)
                (South n)     -> waypointY (+ (-n))
                (East n)      -> waypointX (+ n)
                (West n)      -> waypointX (+ (- n))
                (TurnRight a) -> step2 (TurnLeft (invertAngle a))
                (TurnLeft a)  -> waypointXY (rotate a)
                (Forward n)   -> forward n
  where
    -- move the ship
    forward n ((sx, sy), (wx, wy)) = ((sx + n * wx, sy + n * wy), (wx, wy))
    -- move the waypoint
    waypointX  f (ship, (x, y)) = (ship, (f x,   y))
    waypointY  f (ship, (x, y)) = (ship, (  x, f y))
    waypointXY f (ship, (x, y)) = (ship, f (x, y))
    rotate   D0 (x, y) = ( x,  y)
    rotate  D90 (x, y) = (-y,  x)
    rotate D180 (x, y) = (-x, -y)
    rotate D270 (x, y) = ( y, -x)

solve2 :: [Instruction] -> Ship
solve2 = fst . foldl' (flip step2) ((0, 0), (10, 1))

-- | >>> part2 "F10\nN3\nF7\nR90\nF11"
-- 286
-- >>> part2 <$> readFile "input/2020/day12.txt"
-- 66614
part2 :: String -> Int
part2 input = let (x, y) = solve2 $ parseInput input
              in abs x + abs y



{- I actually did this in perl first:

# part 1
$ perl -lne 'BEGIN { $h = 0; %d=(0=>"E", 90=>"N", 180=>"W", 270=>"S"); %s=(N=>0, S=>0, E=>0, W=>0) }
                if (/L(.*)/) {
                    $h = ($h + $1) % 360
                } elsif (/R(.*)/) {
                    $h = ($h - $1) % 360
                } elsif (/F(.*)/) {
                    $s{ $d{$h} } += $1
                } elsif (/(.)(.*)/) {
                    $s{$1} += $2
                };
            END { print abs($s{N} - $s{S}) + abs($s{E} - $s{W}) }' input/2020/day12.txt
820

# part 2
$ perl -lne 'BEGIN { @s = (0, 0); @w = (10, 1) }
                if (/N(.*)/) {
                    $w[1] += $1
                } elsif (/S(.*)/) {
                    $w[1] -= $1
                } elsif (/E(.*)/) {
                    $w[0] += $1
                } elsif (/W(.*)/) {
                    $w[0] -= $1
                } elsif (/F(.*)/) {
                    $s[0] += $1 * $w[0]; $s[1] += $1 * $w[1]
                } elsif (/R270|L90/) {
                    @w = (-$w[1], $w[0])
                } elsif (/R180|L180/) {
                    @w=(-$w[0], -$w[1])
                } elsif (/R90|L270/) {
                    @w=($w[1], -$w[0])
                } else {
                    print
                };
            END { print abs($s[0]) + abs($s[1]) }' input/2020/day12.txt
66614

-}
