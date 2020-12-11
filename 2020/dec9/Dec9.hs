-- Learnt lots.
-- The (&) operator
-- tails and init
--
-- Not a very quick solution, but quick enough.
module Dec9 where

import Data.Maybe
import Data.List
import Data.Function ((&))


-- trivial version of Dec1.
solveDec1 :: [Int] -> Int -> Maybe Int
solveDec1 xs x =
  listToMaybe [ x | a <- xs, b <- xs, a + b == x]


-- n-length slice of xs starting at i
slice xs n i = take n $ drop (i - n) xs

-- find a tuple ys:y where ys is n items long, ending at position i
getTestCase :: Int -> [a] -> Int -> ([a], a)
getTestCase n xs i =
  (slice xs n i, xs !! i)


-- This was dumb. I could have just iterated over slices (n+1) elements long...
-- Oh well. Fun to write.
mapSlices :: Int -> ([a] -> a -> b) -> [a] -> [b]
mapSlices n f xs =
  map (uncurry f . getTestCase n xs) positions
  where
    positions = [n..length xs - 1]


hasSummingPair :: [Int] -> Int -> Maybe Int
hasSummingPair xs x = if isNothing (solveDec1 xs x) then Just x else Nothing

-- util:
checkAll n = mapSlices n hasSummingPair

-- find the first pair which sums to n
solve1 :: Int -> [Int] -> Int
solve1 n = head . catMaybes . mapSlices n hasSummingPair



-- List segments (non-null continuous sublists)
segments :: [a] -> [[a]]
segments = filter (not . null) . concatMap inits . tails


-- Find a segment in xs that sums to n, assuming that the segment occurs before
-- the element n. Used the (&) operator just for fun. It's like ($) but
-- backwards, so slightly more readable in a pipeline.
findSummingRange :: Int -> [Int] -> [Int]
findSummingRange n xs =
  take (fromJust $ elemIndex n xs) xs
  & segments
  & find ((==n) . sum)
  & fromJust


solve2 :: Int -> [Int] -> Int
solve2 n xs = minimum set + maximum set
  where
    set = findSummingRange (solve1 n xs) xs


---

td :: [Int]
td = [35,
      20,
      15,
      25,
      47,
      40,
      62,
      55,
      65,
      95,
      102,
      117,
      150,
      182,
      127, -- wrong
      219,
      299,
      277,
      309,
      576]

test1 = solve1 5 td == 127
test2 = solve2 5 td == 62

main = do
  dat <- readFile "data.txt"
  let xs = [ read x :: Int | x <- lines dat ]

  print $ solve1 25 xs
  -- 26796446

  print $ solve2 25 xs
  -- 3353494
