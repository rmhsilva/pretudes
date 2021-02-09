
module Dec23 where

import Data.List (elemIndex)
import Data.Maybe
import Control.Monad (forM_, replicateM, (>>=))
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray)
import Data.STRef (newSTRef, writeSTRef, readSTRef, STRef)
import Data.Array.MArray (MArray, newArray, readArray, writeArray, getElems)


solve1 = concatMap show . rotate1 . moveN 101

rotate1 xs = drop (pos+1) xs ++ take pos xs
  where
    pos = fromJust $ 1 `elemIndex` xs

step :: [Int] -> [Int]
step (x:xs) =
  take (destPos+1) fromFourth ++ slice ++ drop (destPos + 1) fromFourth ++ [x]
  where
    n = maximum fromFourth
    slice = take 3 xs
    fromFourth = drop 3 xs
    destElt = head $ filter (`elem` fromFourth) $ map (\a -> (x - a) `wrap` n) [1..5]
    destPos = fromJust $ destElt `elemIndex` fromFourth

wrap x n | x > 0     = x
         | x == 0    = n
         | otherwise = x `mod` n


moveN n = last . take n . iterate step


--

type Ptr = Int
type MyLL s = (STUArray s Int Int, STUArray s Int Ptr)  -- values, ptrs

move3 :: ST s (MyLL s) -> Ptr -> Ptr -> ST s ()
move3 ll a f = do
  -- a,b,c,d,e,...,f,g --> a,e,...f,b,c,d,g
  (_,ptrs) <- ll
  b <- readArray ptrs a
  c <- readArray ptrs b
  d <- readArray ptrs c
  e <- readArray ptrs d
  writeArray ptrs a e
  --
  g <- readArray ptrs f
  writeArray ptrs f b
  writeArray ptrs d g
  return ()

take3 :: ST s (MyLL s) -> Int -> ST s [Int]
take3 ll i = do
  (vals, ptrs) <- ll
  pa <- readArray ptrs i
  a <- readArray vals pa
  pb <- readArray ptrs pa
  b <- readArray vals pb
  pc <- readArray ptrs pb
  c <- readArray vals pc
  return [a,b,c]

findDest x slice n =
  head $ filter (`notElem` slice) $ map (\a -> (x - a) `wrap` n) [1..5]

posOf destVal | destVal < 10 = do return $ fromJust $ destVal `elemIndex` input
              | otherwise = do return $ destVal - 1


step' :: Int -> ST s (STRef s Int, MyLL s) -> ST s ()
step' n state = do
  (i, ll) <- state
  let (vals,ptrs) = ll

  pos <- readSTRef i

  current <- readArray vals pos
  slice <- take3 (pure ll) pos
  let destVal = findDest current slice n
  destPos <- posOf destVal
  move3 (pure ll) pos destPos
  next <- readArray ptrs pos
  writeSTRef i next


moveN' :: Int -> [Int] -> [Int]
moveN' n xs = runST $ do
  let size = maximum xs
  vals <- newArray (0, size-1) 0 :: ST s (STUArray s Int Int)
  ptrs <- newArray (0, size-1) 0 :: ST s (STUArray s Int Ptr)
  iref <- newSTRef 0
  let state = pure (iref, (vals,ptrs))

  forM_ (zip xs [0..]) $ \(x,i) -> do
    writeArray vals i x
    writeArray ptrs i ((i+1) `mod` size)

  n `replicateM` step' size state

  take3 (pure (vals,ptrs)) 0 -- 0 is index of '1'


solve2 = product $ take 2 $ moveN' 10000000 input2
-- several mins to run...
-- 111057672960


--

td    = [3, 8, 9, 1, 2, 5, 4, 6, 7] :: [Int] -- 67384529

input = [1, 5, 7, 6, 2, 3, 9, 8, 4] :: [Int] -- 58427369

td2    = td    ++ [10..1000000]
input2 = input ++ [10..1000000]
