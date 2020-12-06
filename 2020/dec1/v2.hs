-- Smarter and way faster than the simple.hs

import Data.List (sort)
import Data.Maybe
import Data.Set (fromList)


td = [4, 2, 3, 6, 1] :: [Int]
-- td = [1721, 979, 366, 299, 675, 1456] :: [Int]


fillMaybes :: Int -> Int -> [Maybe Int]
fillMaybes a b = replicate (b - a - 1) Nothing ++ [Just b]

makeSparse :: Int -> [Int] -> [Maybe Int]
makeSparse n lst =
  take (n - 1) $ concat $ zipWith fillMaybes (0 : sorted) (sorted ++ [n+1])
  where
    sorted = sort lst


-- find bs in lst where a + b == n
findPairs :: [Maybe Int] -> Int -> Int -> [Int]
findPairs lst n a =
  filter (\b -> a + b == n) $ catMaybes lst --truncated
  -- where
  --   truncated = take ((n `div` 2) - a) lst


-- Find a pair of numbers in sparse that sums to len(sparse)
findSolutionPair :: [Maybe Int] -> Maybe [Int]
findSolutionPair sparse =
  listToMaybe $
  filter ((> 1) . length) $
  map (\x -> x : findPairs secondHalf n x) $
  catMaybes firstHalf
  where
    n = length sparse + 1
    firstHalf = take (n `div` 2) sparse
    secondHalf = drop (n `div` 2) sparse


-- Find the winning pair and multiply the elements
solve :: Int -> [Int] -> Int
solve n lst =
  product $ fromJust $ findSolutionPair $ makeSparse n lst



-- Find triples for the solution
findSolutionTriples :: [Maybe Int] -> [[Int]]
findSolutionTriples sparse =
  filter ((> 2) . length . fromList) $ -- filter sums with duplicated numbers
  map (\x -> x : fromMaybe [] (findSolutionPair (take (n - x - 1) sparse))) $
  catMaybes firstChunk
  where
    n = length sparse + 1
    -- Start from the largest number so that the mapped call to findSolutionPair
    -- can get a slice from the smallest
    firstChunk = take (2 * (n `div` 3)) $ reverse sparse


-- Stretch goal: Find the 3 numbers that sum to 2020 and multiply them
stretch :: Int -> [Int] -> Int
stretch n lst =
  product $
  head $
  findSolutionTriples (makeSparse n lst)


-- NOTE: There is a generalisation between findSolutionPair and
-- findSolutionTriples. With a bit more thought this could be nice and elegant.


main = do
  dat <- readFile "data.txt"
  -- print $ map (\x -> read x :: Int) (lines dat)

  print . solve 2020 $ map (\x -> read x :: Int) (lines dat)
  -- answer: 538464

  print . stretch 2020 $ map (\x -> read x :: Int) (lines dat)
  -- answer: 278783190
