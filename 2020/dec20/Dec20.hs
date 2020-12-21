
module Dec20 where

import Data.List.Split (splitOn)
import Data.Graph (Graph, graphFromEdges)
import Text.Regex.PCRE (mrSubList, mrSubList, MatchResult, (=~))
import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.Set as Set


type Edge  = String
type Tiles = Map Int [Edge]
-- type Final = Graph           -- vertices are Int
-- type Final = Map Int [Int]


-- connectedTo borders edge =

-- shared tiles edge

edgeTiles :: Tiles -> Tiles
edgeTiles tiles = M.filter ((==2) . length . filter id . map (isBorder tiles)) tiles

edgeEq a b
  = a==b || a==reverse b  -- edges can be flipped

numConnected :: Tiles -> Edge -> Int
numConnected edges edge = length $ filter (edgeEq edge) $ allEdges edges

isBorder :: Tiles -> Edge -> Bool
isBorder edges = (==1) . numConnected edges

allEdges = concat . M.elems

tileWithEdge edge = M.filter (edge `elem`)


-- checkUniqueMatches tiles =
--   let all = allEdges tiles in
--     (length (Set.fromList all) == length all - (length all `div` 4)
--       ||
--       error "Shared edges are not unique!")

solve = product . M.keys . edgeTiles

--

parseTile :: [String] -> (Int, [Edge])
parseTile tileLines =
  (tileId, edges)
  where
    match  = mrSubList (head tileLines =~ "Tile ([0-9]*):" :: MatchResult String)
    tileId = read (head match) :: Int
    tile   = drop 1 tileLines
    edges  = [head tile, map head tile, map last tile, last tile]

parse :: [String] -> Tiles
parse = M.fromList . map parseTile . splitOn [""]


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


expected = [ [1951, 2311, 3079],
             [2729, 1427, 2473],
             [2971, 1489, 1171] ] -- 20899048083289

test :: IO ()
test = do
  dat <- readFile "td.txt"
  -- print $ parseTile $ head $ splitOn [""] $ lines dat
  let parsed = parse $ lines dat
  -- print $ checkUniqueMatches parsed
  let all = allEdges parsed

  print $ numConnected parsed (head all)
  print $ isBorder parsed (head all)

  print $ numConnected parsed (all !! 1)
  print $ isBorder parsed (all !! 1)

  print $ edgeTiles parsed
  print $ map (isBorder parsed) (parsed ! 1489)
  mapM_ print $ parsed ! 1489

  -- print $ tileWithEdge "#...##.#.." parsed


main = do
  dat <- readFile "data.txt"
  let parsed = parse $ lines dat
  -- print $ checkUniqueMatches parsed

  print $ solve parsed
  -- 12519494280967
