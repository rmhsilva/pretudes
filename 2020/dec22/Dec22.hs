
module Dec22 where

import Data.List.Split (splitOn)


solve1 = sum . zipWith (*) [1..] . reverse . winnerCards . until finished play


type Deck = [Int]


finished :: (Deck,Deck) -> Bool
finished (p1,p2) = null p1 || null p2


play :: (Deck,Deck) -> (Deck,Deck)
play (p1:p1s, p2:p2s) | p1 > p2   = (p1s ++ [p1, p2], p2s)
                      | otherwise = (p1s, p2s ++ [p2, p1])

winnerCards :: (Deck,Deck) -> Deck
winnerCards (p1,p2) | null p2   = p1
                    | otherwise = p2


readInt x = read x :: Int

parse input = (map readInt (drop 1 p1s), map readInt (drop 1 p2s))
  where
    (p1s:p2s:_) = splitOn [""] input

td = parse ["Player 1:", "9", "2", "6", "3", "1",
            "",
            "Player 2:", "5", "8", "4", "7", "10"]


main = do
  dat <- readFile "data.txt"
  let input = parse $ lines dat

  print $ solve1 input
  -- print $ solve2 input
