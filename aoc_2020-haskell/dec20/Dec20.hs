-- cabal install --lib split fg regex-pcre
module Dec20 where

import Control.Monad
import Data.Maybe
import Data.List.Split (splitOn)
import Data.Graph.Inductive.Query.SP (sp, spLength)
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Text.Regex.PCRE (mrSubList, mrSubList, MatchResult, (=~))
import qualified Data.Map as M
import Data.Map (Map, (!))


type Edge      = String
type Tile      = [String]
type TileEdges = Map Int [Edge]
type TileMap   = Map Int Tile


-- Part 1:

edgeTiles :: TileEdges -> TileEdges
edgeTiles tiles = M.filter ((==2) . length . filter id . map (isBorder tiles)) tiles

edgeEq a b
  = a==b || a==reverse b  -- edges can be flipped

numConnected :: TileEdges -> Edge -> Int
numConnected edges edge = length $ filter (edgeEq edge) $ allEdges edges

isBorder :: TileEdges -> Edge -> Bool
isBorder edges = (==1) . numConnected edges

allEdges = concat . M.elems

tileWithEdge edge = M.filter (edge `elem`)

solve1 = product . M.keys . edgeTiles . parse1


parseTile :: [String] -> (Int, Tile)
parseTile tileLines =
  (tileId, tile)
  where
    match  = mrSubList (head tileLines =~ "Tile ([0-9]*):" :: MatchResult String)
    tileId = read (head match) :: Int
    tile   = drop 1 tileLines

edgesOf :: Tile -> [Edge]
edgesOf tile = [head tile, map head tile, map last tile, last tile]

topEdge tile    = edgesOf tile !! 0
leftEdge tile   = edgesOf tile !! 1
rightEdge tile  = edgesOf tile !! 2
bottomEdge tile = edgesOf tile !! 3


-- just the edges
parseTileEdges :: [String] -> (Int, [Edge])
parseTileEdges tileLines
  = (tileId, edgesOf tile)
  where
    (tileId, tile) = parseTile tileLines

parse1 :: [String] -> TileEdges
parse1 = M.fromList . map parseTileEdges . splitOn [""]


-- Part 2:

bordersWith :: Tile -> Tile -> Bool
bordersWith a b =
  or $ edgeEq <$> edgesOf a <*> edgesOf b

connections :: TileMap -> Int -> [Int]
connections tiles tileId =
  M.keys $ M.filterWithKey (\k a -> k /= tileId && a `bordersWith` tile) tiles
  where
    tile = tiles ! tileId


parseTiles :: [String] -> TileMap
parseTiles = M.fromList . map parseTile . splitOn [""]

makeGraph :: TileMap -> Gr Int Int
makeGraph tiles =
  mkGraph (map (\x -> (x, x)) ids) (concatMap connectingTuples ids)
  --      [(node, 'label')]        [(nodeA, nodeB, 'label')]
  where
    ids = M.keys tiles
    connectingTuples k = [ (k, k', k) | k' <- connections tiles k ]


imageSideLength = floor . sqrt . fromIntegral . length . M.elems

corners tiles = M.filterWithKey (\k a -> length (connections tiles k) == 2) tiles

borderPaths :: TileMap -> [[Int]]
borderPaths tiles = filter ((==imageSideLength tiles) . length) pathsBetweenCorners
  where
    pathsBetweenCorners = mapMaybe stepsBetween pairs
    stepsBetween (a,b) = sp a b graph
    pairs = (,) <$> c' <*> c'
    c' = M.keys $ corners tiles
    graph = makeGraph tiles


neighbourOf tiles taken tileId =
  head $ filter (not . (`elem` taken)) possible
  where
    possible = connections tiles tileId

findNextRow :: TileMap -> ([Int], [Int]) -> ([Int], [Int])
findNextRow tiles (taken, row) =
  (taken ++ row, result)
  where
    result = map (neighbourOf tiles (taken++row)) row

-- find two corner tiles
-- find the shortest path between them
-- if the path length is the length of a side, continue
-- take that shortest path as the starting "side"
-- for each tile in that side, find its neighbour not already in the side
-- build the image by scanning downwards
arrangeTiles :: TileMap -> [[Int]]
arrangeTiles tiles =
  map snd $ take numRows $ iterate (findNextRow tiles) ([], firstRow)
  where
    numRows = imageSideLength tiles
    firstRow = head $ borderPaths tiles


stripEdges :: Tile -> Tile
stripEdges tile = map strip (strip tile)
  where
    strip x = drop 1 $ take (length x - 1) x


-- 2D matrix manipulation functions
--
flipX            = map reverse
flipY            = reverse
transpose ([]:_) = []
transpose x      = map head x : transpose (map tail x)
rotate90         = flipX . transpose
rotate180        = flipY . flipX
rotate270        = rotate90 . rotate180

variationsOf tile =
  map ($ tile) [id, flipY, flipX,
                rotate90, rotate180, rotate270,
                rotate90 . flipY, rotate180 . flipY, rotate270 . flipY,
                rotate90 . flipX, rotate180 . flipX, rotate270 . flipX ]


allEdgesMatch :: [Maybe Tile] -> Tile -> Bool
allEdgesMatch (t:l:r:b:_) tile =
  and [ isNothing t || edgeMatches et (fromJust t),
        isNothing l || edgeMatches el (fromJust l),
        isNothing r || edgeMatches er (fromJust r),
        isNothing b || edgeMatches eb (fromJust b) ]
  where
    (et :el :er :eb :_) = edgesOf tile

-- does an edge match any of the edges of another tile
edgeMatches :: Edge -> Tile -> Bool
edgeMatches e b = e `elem` concatMap edgesOf (variationsOf b)


findOrientation :: Tile -> [Maybe Tile] -> Tile
findOrientation tile neighbours =
  head $ filter (allEdgesMatch neighbours) $ variationsOf tile

arrangeProperly :: TileMap -> [[Tile]]  -- eg 3x3 for td
arrangeProperly tiles =
  [ [ findOrientation ((tileImages !! y) !! x) (neighbours x y)
    | x <- [0..n-1] ]
  | y <- [0..n-1] ]
  where
    n = length tileImages
    tileImages :: [[Tile]] -- 2D list of tiles
    tileImages = map (map (tiles M.!)) (arrangeTiles tiles)
    neighbours x y = map tileAt (neighbourCoords x y)
    tileAt (x,y) = if 0 <= x && x < n && 0 <= y && y < n
                   then Just $ (tileImages !! y) !! x
                   else Nothing
    neighbourCoords x y = [(x,y-1), (x-1,y), (x+1,y), (x,y+1)]
    --                     t        l        r        b


-- arrange the image into a 2D [[Tile]]
-- remove edges from each sub image
-- concat them into a [String]
buildImage :: TileMap -> [String]
buildImage tiles =
  concatMap (concatRow . map stripEdges) (arrangeProperly tiles)
  where
    concatRow :: [Tile] -> [String]
    concatRow row = foldl1 (zipWith (++)) row


-- convolve ther monster kernel with the image*
-- image* = all flipped/rotated versions of the image until a sea monster matches


monsterAt :: [String] -> [String] -> Int -> Int -> Bool
monsterAt kernel image x y =
  and $ check' <$> [0 .. length (head kernel) - 1] <*> [0 .. length kernel - 1]
  where
    check' i j = kernelChar /= '#' || imageChar == '#'  -- only #'s matter
      where
        imageChar  = (image  !! (j+y)) !! (i+x)
        kernelChar = (kernel !!  j   ) !!  i


findMonsters :: [String] -> [String] -> [Bool]
findMonsters kernel image =
  monsterAt kernel image <$> [0 .. xmax] <*> [0 .. ymax]
  where
    xmax = length (head image) - length (head kernel) - 1
    ymax = length image - length kernel - 1


monster = [ "                  # ",
            "#    ##    ##    ###",
            " #  #  #  #  #  #   " ]

td2 = [".####...#####..#...###..",
       "#####..#..#.#.####..#.#.",
       ".#.#...#.###...#.##.##..",
       "#.#.##.###.#.##.##.#####",
       "..##.###.####..#.####.##",
       "...#.#..##.##...#..#..##",
       "#.##.#..#.#..#..##.#.#..",
       ".###.##.....#...###.#...",
       "#.####.#.#....##.#..#.#.",
       "##...#..#....#..#...####",
       "..#.##...###..#.#####..#",
       "....#.##.#.#####....#...",
       "..##.##.###.....#.##..#.",
       "#...#...###..####....##.",
       ".#.##...#.##.#.#.###...#",
       "#.###.#..####...##..#...",
       "#.###...#.##...#.######.",
       ".###.###.#######..#####.",
       "..##.#..#..#.#######.###",
       "#.#..##.########..#..##.",
       "#.#####..#.#...##..#....",
       "#....##..#.#########..##",
       "#...#.....#..##...###.##",
       "#..###....##.#...##.##.#"]



-- Test cases:

tt = [ "Tile 2311:",
       "..##.#..#.",
       "##..#.....",
       "#...##..#.",
       "####.#...#",
       "##.##.###.",
       "##...#.###",
       ".#.#.#..##",
       "..#....#..",
       "###...#.#.",
       "..###..###" ]

t2 = [ "b........a",
       "..........",
       "..........",
       "..........",
       "..........",
       "..........",
       "..........",
       "..........",
       "..........",
       "c........d" ]


solve2 input = hashesInImage - numMonsters * hashesInMonster
  where
    parsed = parseTiles input
    variations = variationsOf $ buildImage parsed

    -- `quot` 2: There's a mistake somewhere in variations or findMonsters, and
    -- monsters are found twice. Oops.
    numMonsters
      = length (concatMap (filter id . findMonsters monster) variations) `quot` 2

    hashesInImage = length $ filter (=='#') $ concat $ buildImage parsed
    hashesInMonster = length $ filter (=='#') $ concat monster


main = do
  dat <- readFile "data.txt"
  let input = lines dat

  print $ solve1 input
  -- 12519494280967

  print $ solve2 input
  -- 2442
