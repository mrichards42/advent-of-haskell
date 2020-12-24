{-# LANGUAGE TupleSections #-}

{- Lobby Layout -}
module Advent.Y2020.Day24 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

{- Part 1

Your raft makes it to the tropical island; it turns out that the small crab was
an excellent navigator. You make your way to the resort.

As you enter the lobby, you discover a small problem: the floor is being
renovated. You can't even reach the check-in desk until they've finished
installing the new tile floor.

The tiles are all hexagonal; they need to be arranged in a hex grid with a very
specific color pattern. Not in the mood to wait, you offer to help figure out
the pattern.

The tiles are all white on one side and black on the other. They start with the
white side facing up. The lobby is large enough to fit whatever pattern might
need to appear there.

A member of the renovation crew gives you a list of the tiles that need to be
flipped over (your puzzle input). Each line in the list identifies a single
tile that needs to be flipped by giving a series of steps starting from a
reference tile in the very center of the room. (Every line starts from the same
reference tile.)

Because the tiles are hexagonal, every tile has six neighbors: east, southeast,
southwest, west, northwest, and northeast. These directions are given in your
list, respectively, as e, se, sw, w, nw, and ne. A tile is identified by a
series of these directions with no delimiters; for example, esenee identifies
the tile you land on if you start at the reference tile and then move one tile
east, one tile southeast, one tile northeast, and one tile east.

Each time a tile is identified, it flips from white to black or from black to
white. Tiles might be flipped more than once. For example, a line like esew
flips a tile immediately adjacent to the reference tile, and a line like
nwwswee flips the reference tile itself.

Here is a larger example:

    sesenwnenenewseeswwswswwnenewsewsw
    neeenesenwnwwswnenewnwwsewnenwseswesw
    seswneswswsenwwnwse
    nwnwneseeswswnenewneswwnewseswneseene
    swweswneswnenwsewnwneneseenw
    eesenwseswswnenwswnwnwsewwnwsene
    sewnenenenesenwsewnenwwwse
    wenwwweseeeweswwwnwwe
    wsweesenenewnwwnwsenewsenwwsesesenwne
    neeswseenwwswnwswswnw
    nenwswwsewswnenenewsenwsenwnesesenew
    enewnwewneswsewnwswenweswnenwsenwsw
    sweneswneswneneenwnewenewwneswswnese
    swwesenesewenwneswnwwneseswwne
    enesenwswwswneneswsenwnewswseenwsese
    wnwnesenesenenwwnenwsewesewsesesew
    nenewswnwewswnenesenwnesewesw
    eneswnwswnwsenenwnwnwwseeswneewsenese
    neswnwewnwnwseenwseesewsenwsweewe
    wseweeenwnesenwwwswnew

In the above example, 10 tiles are flipped once (to black), and 5 more are
flipped twice (to black, then back to white). After all of these instructions
have been followed, a total of 10 tiles are black.

Go through the renovation crew's list and determine which tiles they need to
flip. After all of the instructions have been followed, how many tiles are left
with the black side up?

-}

type Grid k = Map k Bool

data HexDir = NE | SW | E | W
  deriving (Show, Eq)
type HexPoint = (Int, Int) -- (NE - SW, E - W)
type HexGrid = Grid HexPoint

pointsToGrid :: [HexPoint] -> HexGrid
pointsToGrid = foldl' flipPoint Map.empty
  where flipPoint m pt = Map.alter (Just . maybe True not) pt m

countBlack :: HexGrid -> Int
countBlack = Map.size . Map.filter id

pHexPoint :: Parser HexPoint
pHexPoint = do
  dirs <- concat <$> M.some pDir
  return (countOf NE dirs - countOf SW dirs, countOf E dirs - countOf W dirs)
  where
    countOf d = length . filter (== d)
    -- Simplify 3-axis directions into 2-axis directions
    pDir :: Parser [HexDir]
    pDir = M.choice [ [NE]    <$ M.string "ne"
                    , [SW]    <$ M.string "sw"
                    , [W, NE] <$ M.string "nw"
                    , [E, SW] <$ M.string "se"
                    , [E]     <$ M.string "e"
                    , [W]     <$ M.string "w"
                    ]

parseInput :: String -> [HexPoint]
parseInput = parseOrError $ pHexPoint `M.sepEndBy` M.newline

-- | >>> part1 "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew"
-- 10
-- >>> part1 <$> readFile "input/2020/day24.txt"
-- 307
part1 :: String -> Int
part1 = countBlack . pointsToGrid . parseInput


{- Part 2

The tile floor in the lobby is meant to be a living art exhibit. Every day, the
tiles are all flipped according to the following rules:

  - Any black tile with zero or more than 2 black tiles immediately adjacent to
    it is flipped to white.
  - Any white tile with exactly 2 black tiles immediately adjacent to it is
    flipped to black.

Here, tiles immediately adjacent means the six tiles directly touching the tile
in question.

The rules are applied simultaneously to every tile; put another way, it is
first determined which tiles need to be flipped, then they are all flipped at
the same time.

In the above example, the number of black tiles that are facing up after the
given number of days has passed is as follows:

    Day 1: 15
    Day 2: 12
    Day 3: 25
    Day 4: 14
    Day 5: 23
    Day 6: 28
    Day 7: 41
    Day 8: 37
    Day 9: 49
    Day 10: 37

    Day 20: 132
    Day 30: 259
    Day 40: 406
    Day 50: 566
    Day 60: 788
    Day 70: 1106
    Day 80: 1373
    Day 90: 1844
    Day 100: 2208

After executing this process a total of 100 times, there would be 2208 black
tiles facing up.

How many tiles will be black after 100 days?

-}


-- Game of life machinery --

-- from day 17
gridStep :: Ord k
         => (k -> [k])                  -- neighbors
         -> (Bool -> Int -> Maybe Bool) -- rule
         -> (Grid k, Set k)             -- (input grid,  changed points)
         -> (Grid k, Set k)             -- (output grid, changed points)
gridStep neighbors rule (g, changed) = (Map.union g' g, Map.keysSet g')
  where
    pts = Set.toList . Set.union changed . Set.fromList $ concatMap neighbors changed
    g' = Map.fromList $ mapMaybe stepFn pts

    stepFn pt = (pt, ) <$> rule v neighborCount
      where v = lookupG g pt
            neighborCount = length $ filter (lookupG g) (neighbors pt)
            lookupG = flip (Map.findWithDefault False)

-- | Runs a Conway-like game a fixed number of steps
runGame :: Ord k
        => Int                         -- steps
        -> (k -> [k])                  -- neighbors
        -> (Bool -> Int -> Maybe Bool) -- rule
        -> Grid k
        -> Grid k
runGame n neighbors rule g =
  let changed = Map.keysSet g -- assume all points are changed to start
  in fst . (!! n) . iterate (gridStep neighbors rule) $ (g, changed)


-- part 2 --

hexNeighbors :: HexPoint -> [HexPoint]
hexNeighbors (ne, e) =
  [ (ne + 1, e    )
  , (ne - 1, e    )
  , (ne    , e + 1)
  , (ne    , e - 1)
  , (ne - 1, e + 1) -- se
  , (ne + 1, e - 1) -- nw
  ]

stepRule :: Bool -> Int -> Maybe Bool
stepRule black neighborCount
  | black && (neighborCount == 0 || neighborCount > 2) = Just False
  | not black && neighborCount == 2 = Just True
  | otherwise = Nothing

-- | >>> part2 "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew"
-- 2208
-- >>> part2 <$> readFile "input/2020/day24.txt"
-- 3787
part2 :: String -> Int
part2 = countBlack
      . runGame 100 hexNeighbors stepRule
      . pointsToGrid
      . parseInput
