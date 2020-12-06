-- Tricky.
--
-- Lessons:
--  * be careful with string extraction
--  * don't be lazy - parse them into a proper data structure from the start
--
-- If you've messed up your validation logic, you'll need to easily debug (ie
-- list all of the extracted fields and their validation results)

module Dec4 where

import Text.Regex.PCRE (mrSubList, MatchResult, (=~))
import Data.List
import Data.Maybe

-- How this SHOULD have been implmented:
-- type Passport = [Field]  -- split on space
-- data Field = Field {name :: String, value :: String}  -- validate separately


splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc


requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

check :: String -> Bool
check pp =
  all ((`isInfixOf` pp) . (++":")) requiredFields

-- ugh.
valid :: String -> Maybe String -> Bool
valid "byr" (Just x) = num >= 1920 && num <= 2002 where num = read x :: Int
valid "iyr" (Just x) = num >= 2010 && num <= 2020 where num = read x :: Int
valid "eyr" (Just x) = num >= 2020 && num <= 2030 where num = read x :: Int
valid "ecl" (Just x) = x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
valid "hgt" (Just x) = okHeight val unit
  where
    okHeight :: Int -> String -> Bool
    okHeight v "cm" = 150 <= v && v <= 193
    okHeight v "in" = 59 <= v && v <= 76
    okHeight _ _ = False
    val = read (take (length x - 2) x) :: Int
    unit = drop (length x - 2) x
valid "hcl" (Just x) = length x == 6
valid "pid" (Just x) = length x == 9
valid "cid" _ = True -- ignored
valid _ Nothing = False
valid f _ = error ("Bad field " ++ f)


extract :: String -> String -> Maybe String
extract f x =
  listToMaybe $ mrSubList result
  where
    result
      | f == "hcl" = x =~ (f ++ ":#([0-9a-f]*)") :: MatchResult String
      | f == "pid" = x =~ (f ++ ":([0-9]*)") :: MatchResult String
      | True = x =~ (f ++ ":([^ \n]*)") :: MatchResult String


allExtractors = map extract requiredFields

extractAll pp = map (\e -> e pp) allExtractors

check2 :: String -> Bool
check2 = and . check2Fields

check2Fields :: String -> [Bool]
check2Fields pp =
  map (\f -> f `isInfixOf` pp && valid f (extract f pp)) requiredFields


getPassports :: String -> [String]
getPassports dat =
  filter ((1<) . length) $ map fix $ splitStr "\n\n" dat
  where
    fix = map (\c -> if c == '\n' then ' ' else c)


solve :: (String -> Bool) -> String -> Int
solve checker = length . filter id . map checker . getPassports


td = unlines ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
              "byr:1937 iyr:2017 cid:147 hgt:183cm",
              "",
              "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
              "hcl:#cfa07d byr:1929",
              "",
              "hcl:#ae17e1 iyr:2013",
              "eyr:2024",
              "ecl:brn pid:760753108 byr:1931",
              "hgt:179cm",
              "",
              "hcl:#cfa07d eyr:2025 pid:166559648",
              "iyr:2011 ecl:brn hgt:59in"]


findValid :: [String] -> [String]
findValid pps =
  map fst $ filter (\(_, checks) -> and checks) $ zip pps (map check2Fields pps)

main = do
  dat <- readFile "data.txt"
  good <- readFile "good.txt"
  bad <- readFile "bad.txt"

  print $ solve check dat
  -- 200

  -- print $ check2Fields $ getPassports bad !! 1

  print $ solve check2 dat
  -- 116

  -- print $ map check2 $ getPassports good
  -- print $ map check2 $ getPassports bad
  -- mapM_ print $
  --     findValid $ getPassports dat

