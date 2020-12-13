-- Chinese remainer theorm
module Dec13 where

import Data.List.Split (splitOn)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Foldable (toList)

type Input1 = (Int, [Int])

-- besout
-- | besout compute extended gcd of two integers. For example : "besout" 13 17 =
-- [4,-3,1] , this means that gcd of 13 and 17 is 1 and 1 could be written as
-- linear combination of 13 and 17 as 1 = 4*13 - 3*17.
-- https://hackage.haskell.org/package/besout-0.2.0.1/docs/src/Bezout.html
besout :: Integer -> Integer -> [Integer]
besout x y = bBesout [1,0,x] [0,1,y]
  where
    bBesout u v =
      case v!!2 of
        0 -> u
        _ -> let q = (u!!2) `div` (v!!2) in bBesout v [u!!k - q * v!!k | k <- [0..2]]


-- offsetFromEstimate
-- | pair each bus with the 'time distance' from the earliest departure time
offsetFromEstimate :: Input1 -> [(Int, Int)]
offsetFromEstimate (earliest, busses) =
  zip busses $ map (\x -> x - (earliest `mod` x)) busses


-- solve1
-- | Simple sort
solve1 :: Input1 -> Int
solve1 = finalMultiply . minimumBy (compare `on` snd) . offsetFromEstimate
  where
    finalMultiply (bus, wait) = bus * abs wait


-- solve2
-- | solve the CRT for [(divisor, remainder)]
-- https://en.wikipedia.org/wiki/Chinese_remainder_theorem#General_case
solve2 :: [(Integer, Integer)] -> Integer
solve2 congruences =
  x `mod` divisorsProduct
  where
    divisorsProduct = product $ map fst congruences
    x = sum $ map sumItem congruences
    sumItem (n, a) =
      a * mm * nn
      where
        nn = divisorsProduct `div` n
        (mm:m:_) = besout nn n


---

parse1 :: [String] -> Input1
parse1 (earliest:busses:_) =
  (read earliest :: Int, [ read x :: Int | x <- splitOn "," busses, x /= "x" ])


parse2 :: [String] -> [(Integer, Integer)]
parse2 (_:busses:_) =
  map readCongruence $
  filter ((/="x") . fst) $
  zip (splitOn "," busses) [0..]
  where
    -- NOTE: the 'offset' (in the problem definition) is not the remainder
    readCongruence (d, offset) = let d' = read d :: Integer in (d', d' - offset)


---

td :: [String]
td = ["939", "7,13,x,x,59,x,31,19"]

p2tds = map parse2 [["", "17,x,13,19"],
                    ["", "67,7,59,61"],
                    ["", "67,x,7,59,61"],
                    ["", "67,7,x,59,61"],
                    ["", "1789,37,47,1889"]]

dat :: [String]
dat = ["1002392",
       "23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,37,x,x,x,x,x,421,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,x,19,x,x,x,x,x,x,x,x,x,29,x,487,x,x,x,x,x,x,x,x,x,x,x,x,13"]


main = do
  print $ solve1 $ parse1 dat
  -- 3789

  print $ solve2 $ parse2 dat
  -- 667437230788118
