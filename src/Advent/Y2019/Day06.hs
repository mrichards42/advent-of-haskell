{- Universal Orbit Map -}
module Advent.Y2019.Day06 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import qualified Data.Char as C
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

{- Part 1

You've landed at the Universal Orbit Map facility on Mercury. Because
navigation in space often involves transferring between orbits, the orbit
maps here are useful for finding efficient routes between, for example, you
and Santa. You download a map of the local orbits (your puzzle input).

Except for the universal Center of Mass (COM), every object in space is in
orbit around exactly one other object. An orbit looks roughly like this:

                  \
                   \
                    |
                    |
AAA--> o            o <--BBB
                    |
                    |
                   /
                  /

In this diagram, the object BBB is in orbit around AAA. The path that BBB
takes around AAA (drawn with lines) is only partly shown. In the map data,
this orbital relationship is written AAA)BBB, which means "BBB is in orbit
around AAA".

Before you use your map data to plot a course, you need to make sure it
wasn't corrupted during the download. To verify maps, the Universal Orbit Map
facility uses orbit count checksums - the total number of direct orbits (like
the one shown above) and indirect orbits.

Whenever A orbits B and B orbits C, then A indirectly orbits C. This chain
can be any number of objects long: if A orbits B, B orbits C, and C orbits D,
then A indirectly orbits D.

For example, suppose you have the following map:

COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L

Visually, the above map of orbits looks like this:

        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \
                I

In this visual representation, when two objects are connected by a line, the
one on the right directly orbits the one on the left.

Here, we can count the total number of orbits as follows:

  - D directly orbits C and indirectly orbits B and COM, a total of 3 orbits.
  - L directly orbits K and indirectly orbits J, E, D, C, B, and COM, a total
    of 7 orbits.
  - COM orbits nothing.

The total number of direct and indirect orbits in this example is 42.

What is the total number of direct and indirect orbits in your map data?

-}

type Planet = String
type Orbit = (Planet, Planet)       -- (child, parent)
type OrbitEdges = Map Planet Planet -- child -> parent

-- | "Center of mass"
com :: String
com = "COM"

pOrbit :: Parser Orbit
pOrbit = do
  a <- planet
  _ <- M.char ')'
  b <- planet
  _ <- M.optional M.newline
  return (b, a)
    where planet = M.takeWhile1P (Just "planet") C.isAlphaNum

parseInput :: String -> OrbitEdges
parseInput = Map.fromList . parseOrError (M.some pOrbit)

-- $setup
-- >>> exData = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
-- >>> exOrbits = parseInput exData

-- | Returns the path from `x` Planet to `com`, the center of mass
-- >>> pathFrom "B" exOrbits
-- ["B", "COM"]
-- >>> pathFrom "L" exOrbits
-- ["L", "K", "J", "E", "D", "C", "B", "COM"]
pathFrom :: Planet -> OrbitEdges -> [Planet]
pathFrom x orbits = if x == com
                      then [x]
                      else x : pathFrom (orbits Map.! x) orbits

-- | >>> totalOrbits exOrbits
-- 42
totalOrbits :: OrbitEdges -> Int
totalOrbits orbits = sum $ map orbitCount (Map.keys orbits)
  where orbitCount x = length (pathFrom x orbits) - 1

-- | >>> readFile "input/2019/day06.txt" >>= return . part1
-- 186597
part1 :: String -> Int
part1 = totalOrbits . parseInput


{- Part 2

Now, you just need to figure out how many orbital transfers you (YOU) need to
take to get to Santa (SAN).

You start at the object YOU are orbiting; your destination is the object SAN
is orbiting. An orbital transfer lets you move from any object to an object
orbiting or orbited by that object.

For example, suppose you have the following map:

COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN

Visually, the above map of orbits looks like this:

                          YOU
                         /
        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \
                I - SAN

In this example, YOU are in orbit around K, and SAN is in orbit around I. To
move from K to I, a minimum of 4 orbital transfers are required:

    K to J
    J to E
    E to D
    D to I

Afterward, the map of orbits looks like this:

        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \
                I - SAN
                 \
                  YOU

What is the minimum number of orbital transfers required to move from the
object YOU are orbiting to the object SAN is orbiting? (Between the objects
they are orbiting - not between YOU and SAN.)

-}

-- $setup
-- >>> exData' = exData ++ "\nK)YOU\nI)SAN"
-- >>> exOrbits' = parseInput exData'

-- | >>> shortestPath "YOU" "SAN" exOrbits'
-- ["YOU","K","J","E","D","I","SAN"]
shortestPath :: Planet -> Planet -> OrbitEdges -> [Planet]
shortestPath a b orbits =
  let aPath = pathFrom a orbits
      bPath = pathFrom b orbits
      pivot = fst . last . takeWhile (uncurry (==)) $ zip (reverse aPath) (reverse bPath)
  in concat [ takeWhile (/= pivot) aPath
            , [pivot]
            , reverse $ takeWhile (/= pivot) bPath
            ]

-- | >>> readFile "input/2019/day06.txt" >>= return . part2
-- 412
part2 :: String -> Int
part2 = subtract 2 -- exclude the starting points
      . subtract 1 -- we want the edge count, not the node count
      . length
      . shortestPath "YOU" "SAN"
      . parseInput
