import Text.Regex.PCRE
import Data.List


data Item = Item {cmin :: Int, cmax :: Int, chr :: Char, passwd :: String}
  deriving Show


-- Convert a line from the data into an Item
parseItem :: String -> Item
parseItem s =
  Item min max (head (vals !! 2)) (vals !! 3)
  where
    result = s =~ "([0-9]*)-([0-9]*) ([a-z]): ([a-z]*)" :: MatchResult String
    vals = mrSubList result
    min = read (head vals) :: Int
    max = read (vals !! 1) :: Int


-- part 1
valid1 :: Item -> Bool
valid1 item =
  cmin item <= count && count <= cmax item
  where
    match = chr item
    count = length $ filter (== match) (passwd item)


-- count number of valid items when the valid-predicate is applied
solve :: (Item -> Bool) -> [Item] -> Int
solve valid items =
  length $ filter id $ map valid items

solve1 = solve valid1


-- part 2! Re-interpret Item
valid2 item =
  (a && not b) || (not a && b)
  where
    p = passwd item
    a = (p !! (cmin item - 1)) == chr item
    b = (p !! (cmax item - 1)) == chr item

solve2 = solve valid2


td = map parseItem ["1-3 a: abcde",
                    "1-3 b: cdefg",
                    "2-9 c: ccccccccc"]

main = do
  dat <- readFile "data.txt"
  -- print $ map (\x -> read x :: Int) (lines dat)
  putStrLn $ unlines $ take 3 $ lines dat
  -- putStrLn $ unlines $ map show $ take 5 $ map parseItem $ lines dat
  -- print $ map valid $ take 5 $ map parseItem $ lines dat

  print $ solve1 $ map parseItem $ lines dat
  -- answer: 636

  print $ solve2 $ map parseItem $ lines dat
  -- answer: 588
