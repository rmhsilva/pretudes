-- GameState!
--
-- Turns out (//) is O(n) where n is the size of the array.
-- https://stackoverflow.com/questions/7289314/how-fast-is-data-array

module Dec15 where

import Data.Array


-- make an array, N items long, storing the 'turn number' of the last time that
-- index was spoken.

data GameState = GameState { memory :: Array Integer (Maybe Integer),
                             turn :: Integer,
                             last_spoken :: Integer }
  deriving Show

initGame :: Integer -> [Integer] -> GameState
initGame size xs = GameState mem (toInteger $ length xs) (last xs)
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

solve :: Integer -> [Integer] -> Integer
solve n xs = last_spoken $ until ((==n) . turn) step (initGame n xs)


---

tests :: [([Integer], Integer, Integer)]
tests = [([0,3,6], 2020, 436),
         ([0,3,6], 30000000, 175594)]

test = map (\(input, count, result) -> solve count input == result) tests

td = [0,3,6] -- 2020 -> 436

input = [1,12,0,20,8,16]


main = do

  print $ solve 2020 input
  -- 273

  -- print $ solve 30000000 input
