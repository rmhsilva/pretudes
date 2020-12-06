-- basic
module Dec6 where

import Data.List (isPrefixOf, sort, group)
import Data.Set (fromList, intersection)


type GroupAnswers = [String]


splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc


-- parse the chunks of input data
parseInput :: [String] -> [GroupAnswers]
parseInput = map lines . splitStr "\n\n" . unlines


-- basic sort and group
countAnyYes :: GroupAnswers -> Int
countAnyYes = length . group . sort . foldr1 (++)


-- basic set intersection
countAllYes :: GroupAnswers -> Int
countAllYes = length . foldr1 intersection . map fromList


---

solve1 = sum . map countAnyYes
solve2 = sum . map countAllYes


td :: [GroupAnswers]
td = parseInput ["abc",
                 "",
                 "a", "b", "c",
                 "",
                 "ab", "ac",
                 "",
                 "a", "a", "a", "a",
                 "",
                 "b"
                ]

main = do
  dat <- readFile "data.txt"
  let answers = parseInput $ lines dat

  -- print $ length answers
  -- 483

  print $ solve1 answers
  -- 6686

  print $ solve2 answers
  -- 3476
