-- This needs a one-many mapping with:
-- constant insertion
-- fast fmap (to count)

-- build the graph, then do a DFS
-- graph: node (bag name) -> [container bags]
-- erm this might not work, it doesn't consider the *number* of bags.

module Dec7 where

import Text.Regex.PCRE (mrSubList, MatchResult, (=~))
import Data.List.Split (splitOn)
import Data.Tree (flatten)
import Data.Graph (Graph, graphFromEdges, transposeG, dfs)
import Data.Set (fromList)
import Data.List
import Data.Maybe (fromJust)

type Bag = String


-- all bags that can contain other bags
fst3 (a,_,_) = a
-- uniqueBags = fromList . (map fst3) . parseAll


-- invert :: [(Bag, Int, Bag)] -> [[(Bag, Int, Bag)]]
-- invert ls =
--   groupBy (\(_,_,a) (_,_,b) -> a == b) ls

-- let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges $ map (toAdjMat . parseLine) td
--
-- dfs (transposeG graph) [fromJust $ vertexFromKey "shiny gold"]
-- [Node {rootLabel = 7,
--        subForest = [Node {rootLabel = 6,
--                           subForest = [Node {rootLabel = 5,
--                                              subForest = []},
--                                        Node {rootLabel = 2,
--                                              subForest = []}]},
--                      Node {rootLabel = 0, subForest = []}]}]




go ls =
  map (filter (/=0)) $
  map flatten $
  dfs (transposeG graph) [fromJust $ vertexFromKey "shiny gold"]
  where
    (graph, _, vertexFromKey) = graphFromEdges $ map (toAdjMat . parseLine) ls



toAdjMat :: (Bag, [(Int, Bag)]) -> (Bag, Bag, [Bag])
toAdjMat (parent, children) =
  (parent, parent, map snd children)


parseChunk :: String -> (Int, Bag)
parseChunk s =
  (count, colour)
  where
    result = s =~ "([0-9]*) (.*) bags?" :: MatchResult String
    vals = mrSubList result
    count = read (vals !! 0) :: Int
    colour = vals !! 1

parseLine :: Bag -> (Bag, [(Int, Bag)])
parseLine s =
  -- map (\(count, child) -> (parent, count, child)) (map parseChunk chunks)
  (parent, map parseChunk chunks)
  where
    parent:rest = splitOn " bags contain " $ take ((length s) - 1) s
    chunks = if rest == ["no other bags"] then [] else splitOn ", " $ head rest


td = ["light red bags contain 1 bright white bag, 2 muted yellow bags.",
      "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
      "bright white bags contain 1 shiny gold bag.",
      "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
      "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
      "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
      "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
      "faded blue bags contain no other bags.",
      "dotted black bags contain no other bags."]


td2 = ["shiny gold bags contain 2 dark red bags.",
       "dark red bags contain 2 dark orange bags.",
       "dark orange bags contain 2 dark yellow bags.",
       "dark yellow bags contain 2 dark green bags.",
       "dark green bags contain 2 dark blue bags.",
       "dark blue bags contain 2 dark violet bags.",
       "dark violet bags contain no other bags."]

main = do
  dat <- readFile "data.txt"  -- 594 lines
  let ls = lines dat

  print $ length $ head $ go ls
  -- 115
