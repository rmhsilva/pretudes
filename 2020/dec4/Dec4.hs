module Dec4 where

import Data.List

-- data Passport = PValid

splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc


check :: String -> Bool
check pp =
  all (`isInfixOf` pp) requiredFields
  where
    requiredFields = map (++":") ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


getPassports :: String -> [String]
getPassports dat =
  filter ((1<) . length) $ map fix $ splitStr "\n\n" dat
  where
    fix = map (\c -> if c == '\n' then ' ' else c)


solve = length . filter id . map check . getPassports


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

main = do
  dat <- readFile "data.txt"
  putStrLn $ unlines $ take 5 $ lines dat

  print $ solve dat
  -- 200
