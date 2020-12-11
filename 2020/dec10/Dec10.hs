{-# LANGUAGE FlexibleContexts #-}
module Dec10 where

-- Goal: sort the numbers such that the difference between successive elements
-- is <4.

import Data.List


-- type Differences = (Int, Int, Int)


-- next [] = error "empty"
-- next [x] = x + 3
-- next (x:rest) = minimum $ filter ((<4) . (x-)) rest

-- customSort :: [Int] -> [Int]
-- customSort = map next . filter (not . null) . tails . sort


-- differences = map sum
adjPairDiffs :: [Int] -> [Int]
adjPairDiffs xs = zipWith (-) (drop 1 xs) (take (n-1) xs)
  where n = length xs


count x = length . filter (==x)


-- TODO convert to bind
distribs :: [Int] -> [(Int, Int)]
distribs xs = [ (v, count v diffs) | v <- [1, 2, 3]]
  where
    diffs = adjPairDiffs (sort xs)


solve1 :: [Int] -> Int
solve1 xs = snd ( ds !! 0 ) * snd ( ds !! 2 )
  where
    ds = distribs $ 0 : xs ++ [maximum xs + 3]


-- how many elements can we take from a list of n consecutive numbers such that
-- no gap is greater than 2
numValidStates n
  | n > 2 = 2^( n - 2 ) - maximum [n - 4, 0]
  | otherwise = 0


solve2 xs = solve2' (0 : xs ++ [maximum xs + 3])
  where
    solve2' =
      product
      . filter (>0) -- ignore the groups that don't have valid states
      . map (numValidStates . (1+) . length) -- count valid states of each group
      . filter (1 `elem`)  -- keep only the groups of difference=1
      . group
      . adjPairDiffs
      . sort


td :: [Int]
td = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
--   [1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19]
--       3  1  1  1  3   1   1   3   1   3
--   (7, 0, 5)

td2 :: [Int]
td2 = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19,
       38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]
--   (22, 0, 10)


main = do
  dat <- readFile "data.txt"
  let xs = [ read x :: Int | x <- lines dat ]

  print $ distribs xs
  -- [(1,63), (2,0), (3,31)]
  -- NOTE: no gaps of 2!

  print $ solve1 xs
  -- 2048

  print $ solve2 xs
  -- 1322306994176
