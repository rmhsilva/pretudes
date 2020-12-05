-- 128 rows (7 bits), 8 columns (3 bits)

module Dec5 where

import Data.Maybe (catMaybes)

type SeatSpec = String

data Seat = Seat {row :: Int, col :: Int}
  deriving (Show, Eq)

type Range = [Int]


seatId :: Seat -> Int
seatId s = 8 * (row s) + (col s)


-- partition with the custom char rules
part :: Char -> Range -> Range
part chr r
  | chr `elem` ['F', 'L'] = take split r
  | chr `elem` ['B', 'R'] = drop split r
  | otherwise = error "bad chr"
  where
    split = (length r) `div` 2


-- recursive binary partition
bsp :: Range -> String -> Int
bsp r s =
  if length r == 1 then head r else bsp (part thisChar r) nextChunk
  where
    thisChar:nextChunk = s


readRow :: String -> Int
readRow = bsp [0..127]

readCol :: String -> Int
readCol = bsp [0..7]

parseSeat :: SeatSpec -> Seat
parseSeat s = Seat r c
  where
    r = readRow (take 7 s)
    c = readCol (drop 7 s)


-- For part 2, note that the taken seat IDs will be a continuous list, since the
-- equation (row * 8 + col) is continuous when col is in [0,7] (which it is).
--
-- So we just need to loop over triples of the seat id range, looking for a gap.


allSeats :: [Seat]
allSeats = [ Seat row col | row <- [0..127], col <- [0..7] ]

allSeatIds :: [Int]
allSeatIds = map seatId allSeats

-- Generate triples of an array
triples :: [a] -> [(a, a, a)]
triples arr =
  zip3 (take (n-2) arr) (drop 1 (take (n-1) arr)) (drop 2 arr)
  where
    n = length arr


-- Generates a continuous (on the natural numbers) list. Just indicates taken.
takenIdRange :: [Seat] -> [Maybe Int]
takenIdRange seats =
  map (\x -> if x `elem` takenIds then Just x else Nothing) allSeatIds
  where
    takenIds = map seatId seats


findGap :: [Seat] -> Int
findGap seats =
  head $ catMaybes $ map isThisMine (triples (takenIdRange seats))
  where
    -- this only works because takenIdRange generates a continuous range
    isThisMine (Just l, Nothing, Just r) = Just (l + 1)
    isThisMine (_, _, _) = Nothing


---
-- Solutions

solve1 :: [Seat] -> Int
solve1 = maximum . map seatId

solve2 :: [Seat] -> Int
solve2 = findGap


-- test data
td :: [Seat]
td = map parseSeat ["BFFFBBFRRR",
                    "FFFBBBFRRR",
                    "BBFFBBFRLL"]

main = do
  dat <- readFile "data.txt"
  let seats = map parseSeat $ lines dat

  print $ solve1 seats
  -- 926

  print $ solve2 seats
  -- 657
