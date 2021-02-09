-- This version is naive and slow.
import Control.Monad
import Data.List (subsequences)


-- test data
td :: [Int]
td = [2, 4, 3, 6]


-- generate subseqs with filterM for fun (not used)
subseqsN :: Int -> [a] -> [[a]]
subseqsN n = filter (\x -> (length x) == n) . filterM (const [True, False])


-- true if list has two elements that sum to 2020
pairsWithSum :: Int -> [Int] -> Bool
pairsWithSum _ [] = False
pairsWithSum _ [_] = False
pairsWithSum _ (_:_:_:_) = False
pairsWithSum n (a:b:_) = a + b == n

-- pairs in a list which sum to n
validPairs :: Int -> [Int] -> [[Int]]
validPairs n = (filter $ pairsWithSum n) . subsequences

-- multiple the elements of the first valid pair
solve :: Int -> [Int] -> Int
solve n = foldr (*) 1 . head . (validPairs n)


toInt :: String -> Int
toInt = read

main = do
  dat <- readFile "data.txt"
  -- print $ map toInt (lines dat)
  print . (solve 2020) $ map toInt (lines dat)
