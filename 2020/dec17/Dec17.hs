
module Dec17 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Data.Maybe


type Coord = (Int,Int,Int)

type Game = Map.Map Coord Bool



-- unit vectors around a point
directions = map tup $ filter (/= [0,0,0]) $ replicateM 3 [-1,0,1]
  where
    tup (a:b:c:_) = (a,b,c)


addCoords (x,y,z) (x',y',z') = (x+x', y+y', z+z')

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList


keysAndNeighbours m =
  mkUniq $ concat $ map (\k -> k : map (addCoords k) directions) (Map.keys m)


next :: Game -> Coord -> Maybe (Coord, Bool)
next m k =
  if stillAlive then Just (k, True) else Nothing
  where
    alive = Map.member k m
    neighbours = map (\x -> Map.member x m) (addCoords k <$> directions)
    numNeighboursAlive = length (filter id neighbours)
    stillAlive = (alive && numNeighboursAlive `elem` [2, 3])
                 || (not alive && numNeighboursAlive == 3)



step :: Game -> Game
step m = Map.fromList $ mapMaybe (next m) (keysAndNeighbours m)


solve1 :: Game -> Int
solve1 = Map.size . last . take 7 . iterate step


parse :: [String] -> Game
parse xs =
  Map.fromList $
  mapMaybe (\(e,x,y) -> if e == '#' then Just ((x,y,0), True) else Nothing) xs'
  where
    width = length (head xs)
    xs' = map (\(e,i) -> (e, i `mod` width, i `div` width)) $ zip (concat xs) [0..]



drawAtZ :: Game -> Int -> String
drawAtZ g z = do
  unlines $
    header :
    [ [if (x,y,z) `Map.member` g then '#' else '.' | x <- [minimum xs..maximum xs]]
    | y <- [minimum ys..maximum ys]]
  where
    header = "\nz=" ++ (show z)
    ks = Map.keys g
    xs = map fst3 ks
    ys = map snd3 ks

fst3 (v,_,_) = v
snd3 (_,v,_) = v
thd3 (_,_,v) = v


draw g =
  map (drawAtZ g) rangeZ
  where
    zs = map thd3 $ Map.keys g
    rangeZ = [minimum zs .. maximum zs]


td = parse [".#.",
            "..#",
            "###"]  -- 112


main = do
  dat <- readFile "data.txt"
  let input = parse $ lines dat
  print $ Map.size input

  print $ solve1 input
  -- 173 is too low
