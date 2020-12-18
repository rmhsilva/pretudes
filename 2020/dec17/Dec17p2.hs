-- just add another dimension why don't you. This is the lazy solution.
--
-- TODO one day - generalise
module Dec17 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Data.Maybe


type Coord = (Int,Int,Int,Int)

type Game = Map.Map Coord Bool



-- unit vectors around a point
directions = map tup $ filter (/= [0,0,0,0]) $ replicateM 4 [-1,0,1]
  where
    tup (a:b:c:d:_) = (a,b,c,d)


addCoords (x,y,z,w) (x',y',z',w') = (x+x', y+y', z+z', w+w')

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList


keysAndNeighbours m =
  mkUniq $ concatMap (\k -> k : map (addCoords k) directions) (Map.keys m)


next :: Game -> Coord -> Maybe (Coord, Bool)
next m k =
  if stillAlive then Just (k, True) else Nothing
  where
    alive = Map.member k m
    neighbours = map (`Map.member` m) (addCoords k <$> directions)
    numNeighboursAlive = length (filter id neighbours)
    stillAlive = (alive && numNeighboursAlive `elem` [2, 3])
                 || (not alive && numNeighboursAlive == 3)



step :: Game -> Game
step m = Map.fromList $ mapMaybe (next m) (keysAndNeighbours m)


solve2 :: Game -> Int
solve2 = Map.size . last . take 7 . iterate step


parse :: [String] -> Game
parse xs =
  Map.fromList $
  mapMaybe (\(e,x,y) -> if e == '#' then Just ((x,y,0,0), True) else Nothing) xs'
  where
    width = length (head xs)
    xs' = zipWith (\ e i -> (e, i `mod` width, i `div` width)) (concat xs) [0..]


td = parse [".#.",
            "..#",
            "###"]  -- 848


main = do
  dat <- readFile "data.txt"
  let input = parse $ lines dat
  print $ Map.size input

  print $ solve2 input
  -- 1620
