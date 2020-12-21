{-# LANGUAGE LambdaCase #-}

{- Jurassic Jigsaw -}
module Advent.Y2020.Day20 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import qualified Advent.Util.Parsing as P
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

{- Part 1

The high-speed train leaves the forest and quickly carries you south. You can
even see a desert in the distance! Since you have some spare time, you might as
well see if there was anything interesting in the image the Mythical
Information Bureau satellite captured.

After decoding the satellite messages, you discover that the data actually
contains many small images created by the satellite's camera array. The camera
array consists of many cameras; rather than produce a single square image, they
produce many smaller square image tiles that need to be reassembled back into a
single image.

Each camera in the camera array returns a single monochrome image tile with a
random unique ID number. The tiles (your puzzle input) arrived in a random
order.

Worse yet, the camera array appears to be malfunctioning: each image tile has
been rotated and flipped to a random orientation. Your first task is to
reassemble the original image by orienting the tiles so they fit together.

To show how the tiles should be reassembled, each tile's image data includes a
border that should line up exactly with its adjacent tiles. All tiles have this
border, and the border lines up exactly when the tiles are both oriented
correctly. Tiles at the edge of the image also have this border, but the
outermost edges won't line up with any other tiles.

For example, suppose you have the following nine tiles:

    Tile 2311:
    ..##.#..#.
    ##..#.....
    #...##..#.
    ####.#...#
    ##.##.###.
    ##...#.###
    .#.#.#..##
    ..#....#..
    ###...#.#.
    ..###..###

    Tile 1951:
    #.##...##.
    #.####...#
    .....#..##
    #...######
    .##.#....#
    .###.#####
    ###.##.##.
    .###....#.
    ..#.#..#.#
    #...##.#..

    Tile 1171:
    ####...##.
    #..##.#..#
    ##.#..#.#.
    .###.####.
    ..###.####
    .##....##.
    .#...####.
    #.##.####.
    ####..#...
    .....##...

    Tile 1427:
    ###.##.#..
    .#..#.##..
    .#.##.#..#
    #.#.#.##.#
    ....#...##
    ...##..##.
    ...#.#####
    .#.####.#.
    ..#..###.#
    ..##.#..#.

    Tile 1489:
    ##.#.#....
    ..##...#..
    .##..##...
    ..#...#...
    #####...#.
    #..#.#.#.#
    ...#.#.#..
    ##.#...##.
    ..##.##.##
    ###.##.#..

    Tile 2473:
    #....####.
    #..#.##...
    #.##..#...
    ######.#.#
    .#...#.#.#
    .#########
    .###.#..#.
    ########.#
    ##...##.#.
    ..###.#.#.

    Tile 2971:
    ..#.#....#
    #...###...
    #.#.###...
    ##.##..#..
    .#####..##
    .#..####.#
    #..#.#..#.
    ..####.###
    ..#.#.###.
    ...#.#.#.#

    Tile 2729:
    ...#.#.#.#
    ####.#....
    ..#.#.....
    ....#..#.#
    .##..##.#.
    .#.####...
    ####.#.#..
    ##.####...
    ##..#.##..
    #.##...##.

    Tile 3079:
    #.#.#####.
    .#..######
    ..#.......
    ######....
    ####.#..#.
    .#...#.##.
    #.#####.##
    ..#.###...
    ..#.......
    ..#.###...

By rotating, flipping, and rearranging them, you can find a square arrangement
that causes all adjacent borders to line up:

    #...##.#.. ..###..### #.#.#####.
    ..#.#..#.# ###...#.#. .#..######
    .###....#. ..#....#.. ..#.......
    ###.##.##. .#.#.#..## ######....
    .###.##### ##...#.### ####.#..#.
    .##.#....# ##.##.###. .#...#.##.
    #...###### ####.#...# #.#####.##
    .....#..## #...##..#. ..#.###...
    #.####...# ##..#..... ..#.......
    #.##...##. ..##.#..#. ..#.###...

    #.##...##. ..##.#..#. ..#.###...
    ##..#.##.. ..#..###.# ##.##....#
    ##.####... .#.####.#. ..#.###..#
    ####.#.#.. ...#.##### ###.#..###
    .#.####... ...##..##. .######.##
    .##..##.#. ....#...## #.#.#.#...
    ....#..#.# #.#.#.##.# #.###.###.
    ..#.#..... .#.##.#..# #.###.##..
    ####.#.... .#..#.##.. .######...
    ...#.#.#.# ###.##.#.. .##...####

    ...#.#.#.# ###.##.#.. .##...####
    ..#.#.###. ..##.##.## #..#.##..#
    ..####.### ##.#...##. .#.#..#.##
    #..#.#..#. ...#.#.#.. .####.###.
    .#..####.# #..#.#.#.# ####.###..
    .#####..## #####...#. .##....##.
    ##.##..#.. ..#...#... .####...#.
    #.#.###... .##..##... .####.##.#
    #...###... ..##...#.. ...#..####
    ..#.#....# ##.#.#.... ...##.....

For reference, the IDs of the above tiles are:

    1951    2311    3079
    2729    1427    2473
    2971    1489    1171

To check that you've assembled the image correctly, multiply the IDs of the
four corner tiles together. If you do this with the assembled tiles from the
                                      example above, you get 1951 * 3079 * 2971
                                      * 1171 = 20899048083289.

-}


-- Tile data --

type TileGrid = Vector Bool

gridSize :: TileGrid -> Int
gridSize g = case V.length g of
               14400 -> 120
               9216 -> 96
               100 -> 10
               64  -> 8
               x -> error $ "Expected standard grid size but got " ++ show x

maxCol :: TileGrid -> Int
maxCol = pred . gridSize

maxRow :: TileGrid -> Int
maxRow = pred . gridSize

data Transform = Front0
               | Front90
               | Front180
               | Front270
               | Back0
               | Back90
               | Back180
               | Back270
               deriving (Show, Eq, Enum, Ord, Bounded)

data Dir = East  -- starting with 0 and going to 270
         | North
         | West
         | South
         deriving (Show, Eq, Enum, Ord, Bounded)

type EdgeMap = Map Dir Int

data Tile = Tile { tileGrid :: TileGrid
                 , tileId :: Int
                 , tileEdges :: EdgeMap
                 , tileTransform :: Transform
                 }
                 deriving (Show, Ord, Eq)

tgAt :: (Int, Int) -> TileGrid -> Bool
tgAt (x, y) g = g V.! (y * w + x)
  where w = gridSize g

getEdge :: Dir -> Tile -> Int
getEdge dir Tile{tileEdges=edges} = edges Map.! dir

_showTile :: Tile -> String
_showTile Tile{tileGrid=g, tileId=tId} =
  "Tile: " ++ show tId ++ "\n" ++
    unlines [ [ showSquare (x, y) | x <- [0..maxCol g] ] | y <- [0..maxRow g] ]
  where showSquare pt = case tgAt pt g of
                          True -> '#'
                          False -> '.'

_showTileWithBorders :: Tile -> String
_showTileWithBorders Tile{tileGrid=g, tileId=tId} =
  "Tile: " ++ show tId ++ "\n" ++
    unlines [ concat [ showSquare (x, y) | x <- [0..maxCol g] ] | y <- [0..maxRow g] ]
  where showSquare' pt = case tgAt pt g of
                          True -> '#'
                          False -> ' '
        showSquare (x, y)
          | x `mod` 10 == 0 || x `mod` 10 == 9 || y `mod` 10 == 0 || y `mod` 10 == 9 =
            "\ESC[38;5;237m" ++ [showSquare' (x, y)] ++ "\ESC[0m"
          | otherwise =
            [showSquare' (x, y)]


-- Construction --

mkEdges :: TileGrid -> EdgeMap
mkEdges g =
  Map.fromList [ (East , tgCol xMax)
               , (North, tgRow 0)
               , (West , tgCol 0)
               , (South, tgRow yMax)
               ]
  where tgRow y = toInt [tgAt (x, y) g | x <- [0..xMax]]
        tgCol x = toInt [tgAt (x, y) g | y <- [0..yMax]]
        xMax = maxCol g
        yMax = maxRow g
        -- number conversion
        toInt = binToIntR . reverse . map (\x -> if x then 1 else 0)
        -- from day 5
        binToIntR :: [Int] -> Int
        binToIntR = \case
                       [] -> 0
                       (x:xs') -> x + 2 * binToIntR xs'

mkTile :: Int -> TileGrid -> Tile
mkTile tId grid = Tile { tileGrid=grid
                       , tileId=tId
                       , tileEdges=mkEdges grid
                       , tileTransform=Front0
                       }

mkTileGrid :: [[Bool]] -> TileGrid
mkTileGrid = V.fromList . concat


-- Transformations --

-- | Rotates a TileGrid 90 degrees (counter-clockwise)
rotate90 :: TileGrid -> TileGrid
rotate90 g = V.fromList [ tgAt (x, y) g | x <- xs, y <- ys ]
 where xs = reverse [0..maxCol g]
       ys = [0..maxRow g]

-- | Reflects a TileGrid across the vertical axis (left to right)
reflect :: TileGrid -> TileGrid
reflect g = V.fromList [ tgAt (x, y) g | y <- ys, x <- xs ]
 where xs = reverse [0..maxCol g]
       ys = [0..maxRow g]

transform :: Transform -> TileGrid -> TileGrid
transform Front0   = id
transform Front90  = rotate90
transform Front180 = rotate90 . rotate90
transform Front270 = rotate90 . rotate90 . rotate90
transform Back0    = reflect
transform Back90   = rotate90 . reflect
transform Back180  = rotate90 . rotate90 . reflect
transform Back270  = rotate90 . rotate90 . rotate90 . reflect

-- | Transforms a Tile using a standard transformation
transformTile :: Transform -> Tile -> Tile
transformTile trans tile@Tile{tileGrid=g} =
  let g' = transform trans g
  in tile{ tileGrid = g'
         , tileTransform = trans
         , tileEdges = mkEdges g'
         }

-- | Generates all possible permutations of a Tile (i.e. all transformations)
tilePermutations :: Tile -> [Tile]
tilePermutations tile = [ transformTile trans tile | trans <- [Front0 ..] ]


-- Parsing --

pTileGrid :: Parser TileGrid
pTileGrid = do
  mkTileGrid <$> pRow `P.sepBy1NG` M.newline
  where pRow = M.some pSquare
        pSquare = M.choice [ True  <$ M.char '#'
                           , False <$ M.char '.'
                           ]

pTile :: Parser Tile
pTile = do
  tId <- M.string "Tile " *> L.decimal <* M.string ":\n"
  grid <- pTileGrid
  return $ mkTile tId grid

parseInput :: String -> [Tile]
parseInput = parseOrError $ pTile `P.sepByNG` M.count 2 M.newline


-- Layout --

type Point = (Int, Int)
type Layout = Map Point Tile
type BagOfTiles = Map Int [Tile]

bagFromTiles :: [Tile] -> BagOfTiles
bagFromTiles tiles =
  Map.fromList [ (tileId t, tilePermutations t) | t <- tiles ]

nextPoint :: Layout -> Point
nextPoint l
  | l == Map.empty = (0, 0)
  | otherwise = case fst $ Map.findMax l of
                  -- we have 144 tiles, so that's a 12 x 12 grid
                  (x, 11) -> (x + 1, 0)
                  (x, y)  -> (x, y + 1)

-- | Places a Tile (and all its permutations) into a Layout at a given Point.
-- Returns a list of all valid layouts (or the empty list if it is not possible
-- to place the Tile).
-- I'm sure there's a faster way to do this (e.g. by grouping tiles by edge),
-- but this method is fast enough (around half a second) even though it's
-- basically brute force.
placeTile :: Layout -> Point -> [Tile] -> [Layout]
placeTile layout (x, y) permutations =
  [ Map.insert (x, y) t layout | t <- permutations
                               , tileFits t
                               ]
  where edgeN = getEdge South <$> layout Map.!? (x     , y - 1)
        edgeW = getEdge East  <$> layout Map.!? (x - 1 , y    )
        northMatches t = case edgeN of
                           Nothing -> True
                           Just n -> getEdge North t == n
        westMatches t = case edgeW of
                          Nothing -> True
                          Just w -> getEdge West t == w
        tileFits t = northMatches t && westMatches t

placeNextTile :: (Layout, BagOfTiles) -> [(Layout, BagOfTiles)]
placeNextTile (layout, tileBag)
  | Map.null tileBag = [(layout, tileBag)] -- done
  | otherwise = do
    (tId, tile) <- Map.assocs tileBag
    l' <- placeTile layout pt tile
    return (l', Map.delete tId tileBag)
  where pt = nextPoint layout

layoutTimes :: Int -> [(Layout, BagOfTiles)] -> [(Layout, BagOfTiles)]
layoutTimes n x
  | n > 0 = layoutTimes (n - 1) (concatMap placeNextTile x)
  | otherwise = x

-- | Places all Tiles into a Layout. Returns all valid layouts.
placeTiles :: [Tile] -> [Layout]
placeTiles tiles =
  let inits = placeNextTile (Map.empty, bagFromTiles tiles)
  in map fst . layoutTimes (length tiles) $ inits

-- | >>> part1 <$> readFile "input/2020/day20.txt"
-- 5775714912743
part1 :: String -> Int
part1 input =
  let tiles = parseInput input
      layout = head $ placeTiles tiles
      ((xMax, yMax), _) = Map.findMax layout
      corners = map (layout Map.!) [(0, 0), (xMax, yMax), (0, yMax), (xMax, 0)]
  in product $ map tileId corners


{- Part 2

Now, you're ready to check the image for sea monsters.

The borders of each tile are not part of the actual image; start by removing
them.

In the example above, the tiles become:

    .#.#..#. ##...#.# #..#####
    ###....# .#....#. .#......
    ##.##.## #.#.#..# #####...
    ###.#### #...#.## ###.#..#
    ##.#.... #.##.### #...#.##
    ...##### ###.#... .#####.#
    ....#..# ...##..# .#.###..
    .####... #..#.... .#......

    #..#.##. .#..###. #.##....
    #.####.. #.####.# .#.###..
    ###.#.#. ..#.#### ##.#..##
    #.####.. ..##..## ######.#
    ##..##.# ...#...# .#.#.#..
    ...#..#. .#.#.##. .###.###
    .#.#.... #.##.#.. .###.##.
    ###.#... #..#.##. ######..

    .#.#.### .##.##.# ..#.##..
    .####.## #.#...## #.#..#.#
    ..#.#..# ..#.#.#. ####.###
    #..####. ..#.#.#. ###.###.
    #####..# ####...# ##....##
    #.##..#. .#...#.. ####...#
    .#.###.. ##..##.. ####.##.
    ...###.. .##...#. ..#..###

Remove the gaps to form the actual image:

    .#.#..#.##...#.##..#####
    ###....#.#....#..#......
    ##.##.###.#.#..######...
    ###.#####...#.#####.#..#
    ##.#....#.##.####...#.##
    ...########.#....#####.#
    ....#..#...##..#.#.###..
    .####...#..#.....#......
    #..#.##..#..###.#.##....
    #.####..#.####.#.#.###..
    ###.#.#...#.######.#..##
    #.####....##..########.#
    ##..##.#...#...#.#.#.#..
    ...#..#..#.#.##..###.###
    .#.#....#.##.#...###.##.
    ###.#...#..#.##.######..
    .#.#.###.##.##.#..#.##..
    .####.###.#...###.#..#.#
    ..#.#..#..#.#.#.####.###
    #..####...#.#.#.###.###.
    #####..#####...###....##
    #.##..#..#...#..####...#
    .#.###..##..##..####.##.
    ...###...##...#...#..###

Now, you're ready to search for sea monsters! Because your image is monochrome,
a sea monster will look like this:

                      # 
    #    ##    ##    ###
     #  #  #  #  #  #   

When looking for this pattern in the image, the spaces can be anything; only
the # need to match. Also, you might need to rotate or flip your image before
it's oriented correctly to find sea monsters. In the above image, after
flipping and rotating it to the appropriate orientation, there are two sea
monsters (marked with O):

    .####...#####..#...###..
    #####..#..#.#.####..#.#.
    .#.#...#.###...#.##.O#..
    #.O.##.OO#.#.OO.##.OOO##
    ..#O.#O#.O##O..O.#O##.##
    ...#.#..##.##...#..#..##
    #.##.#..#.#..#..##.#.#..
    .###.##.....#...###.#...
    #.####.#.#....##.#..#.#.
    ##...#..#....#..#...####
    ..#.##...###..#.#####..#
    ....#.##.#.#####....#...
    ..##.##.###.....#.##..#.
    #...#...###..####....##.
    .#.##...#.##.#.#.###...#
    #.###.#..####...##..#...
    #.###...#.##...#.##O###.
    .O##.#OO.###OO##..OOO##.
    ..O#.O..O..O.#O##O##.###
    #.#..##.########..#..##.
    #.#####..#.#...##..#....
    #....##..#.#########..##
    #...#.....#..##...###.##
    #..###....##.#...##.##.#

Determine how rough the waters are in the sea monsters' habitat by counting the
number of # that are not part of a sea monster. In the above example, the
habitat's water roughness is 273.

How many # are not part of a sea monster?

-}

type Pattern = [Point]

readPattern :: String -> Pattern
readPattern = concat . zipWith readRow [0..] . lines
  where readRow :: Int -> String -> [Point]
        readRow y = catMaybes . zipWith (read1 y) [0..]
        read1 y x '#' = Just (x, y)
        read1 _ _ _   = Nothing

seaMonster :: [Point]
seaMonster = readPattern $ unlines [ "                  # "
                                   , "#    ##    ##    ###"
                                   , " #  #  #  #  #  #   "
                                   ]

-- | Combines a Layout of tiles into a single large Tile
-- >>> putStrLn =<< _showTileWithBorders . combineTiles 0 . head . placeTiles . parseInput <$> readFile "input/2020/day20.txt"
-- ()
combineTiles :: Int -> Layout -> Tile
combineTiles border layout =
  mkTile 0 $ V.fromList [ squareAt (lx, ly) (gx, gy)
                        | ly <- [0..lyMax]
                        , gy <- [border..gyMax-border]
                        , lx <- [0..lxMax]
                        , gx <- [border..gxMax-border]
                        ]
  where squareAt layoutPt gridPt = tgAt gridPt $ tileGrid $ layout Map.! layoutPt
        ((lxMax, lyMax), _) = Map.findMax layout
        grid = tileGrid $ layout Map.! (0, 0)
        gxMax = maxCol grid
        gyMax = maxRow grid

-- | Finds all instances of Pattern in a Tile, returning the top left corner of
-- each match.
patternSearch :: Pattern -> Tile -> [Point]
patternSearch pat Tile{tileGrid=g} =
  [ (x, y) | x <- [0..xMax], y <- [0..yMax]
           , match (x, y) ]
  where match (x, y) = all (\(dx, dy) -> tgAt (x + dx, y + dy) g) pat
        xMax = maxCol g - maximum (map fst pat)
        yMax = maxRow g - maximum (map snd pat)

-- | >>> part2 <$> readFile "input/2020/day20.txt"
-- 1836
part2 :: String -> Int
part2 input =
  let bigTile = combineTiles 1 . head . placeTiles . parseInput $ input
      monsterCount = length . head . dropWhile (== []) . map (patternSearch seaMonster) $ tilePermutations bigTile
      tileCount = length . filter id . V.toList $ tileGrid bigTile
  in tileCount - monsterCount * length seaMonster
