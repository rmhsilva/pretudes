-- GameState!
--
-- Turns out (//) is O(n) where n is the size of the array.
-- https://stackoverflow.com/questions/7289314/how-fast-is-data-array
--
-- Data.Map won't help us here either. Haskell is NOT the right language for
-- today's task. ST is *disgusting*.

module Dec15 where

import Data.Array
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, writeSTRef, readSTRef)
import Data.Array.ST (STUArray)
import Data.Array.MArray (MArray, newArray, readArray, writeArray)

-- PART 2

-- Use two mutable arrays to track if and where numbers have been seen

solve2 :: Int -> [Int] -> Int
solve2 size xs = runST $ do
  seen_before <- newArray (0, size-1) False :: ST s (STUArray s Int Bool)
  seen_at     <- newArray (0, size-1) 0     :: ST s (STUArray s Int Int)

  forM_ (zip xs [0..]) $ \(x,i) -> do
    writeArray seen_before x True
    writeArray seen_at x i

  last_spoken <- newSTRef (last xs)

  forM_ [(length xs) .. size - 1] $ \turn -> do
    last <- readSTRef last_spoken
    seen <- readArray seen_before last
    at   <- readArray seen_at last

    let current_number = if not seen then 0 else turn - 1 - at

    writeSTRef last_spoken current_number
    writeArray seen_at last (turn - 1)
    writeArray seen_before last True

  readSTRef last_spoken


--- PART 1

-- make an array, N items long, storing the 'turn number' of the last time that
-- index was spoken.

data GameState = GameState { memory :: Array Int (Maybe Int),
                             turn :: Int,
                             last_spoken :: Int }
  deriving Show

initGame :: Int -> [Int] -> GameState
initGame size xs = GameState mem (length xs) (last xs)
  where
    mem = listArray (0, size-1) (repeat Nothing) // zip xs (Just <$> [0..])


step :: GameState -> GameState
step game@(GameState memory turn last_spoken) =
  game { turn = turn + 1,
         last_spoken = this_num,
         memory = memory // [(last_spoken, Just $ turn - 1)] }
  where
    this_num = case memory ! last_spoken of
      Nothing -> 0
      Just x -> turn - 1 - x


solve :: Int -> [Int] -> Int
solve n xs = last_spoken $ until ((==n) . turn) step (initGame n xs)


---
-- https://github.com/haskelling/aoc2020/blob/main/15b.hs
-- Just to benchmark.
--
-- Slightly slower than my solve2 version. And also crashes. Stack overflow. It
-- has the same problem as the basic solve - keeps recursing and making new
-- maps. The ST version is still the best.

-- import qualified Data.IntMap as M

-- f' :: Int -> [Int] -> Int
-- f' nn xs = get nn
--   where
--     l = length xs
--     get i = if i < l then xs !! (i - 1) else get' i
--     get' target = step (target - l) (last xs) (l - 1) (M.fromList $ zip (init xs) [0..])
--     step 0 y _ _ = y
--     step target' y i m =
--       let y' = case m M.!? y of
--                  Just n  -> i - n
--                  Nothing -> 0
--       in  step (target' - 1) y' (i + 1) (M.insert y i m)


---

td = [0,3,6] :: [Int] -- 2020 -> 436, 30000000 -> 175594

input = [1,12,0,20,8,16] :: [Int]


main = do

  print $ solve 2020 input
  -- 273

  print $ solve2 30000000 input
  -- 47205
