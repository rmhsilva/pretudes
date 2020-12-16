--
module Dec16 where

import Data.List.Split (splitOn)
import Text.Regex.PCRE (mrSubList, MatchResult, (=~))


data Rule = Rule { name :: String, a :: Int, b :: Int, c :: Int, d :: Int }
  deriving Show


isTicketValidForRules :: [Rule] -> [Int] -> Bool
isTicketValidForRules rules ticket =
  any (isTicketValidForRule ticket) rules

isTicketValidForRule :: [Int] -> Rule -> Bool
isTicketValidForRule ticket rule =
  all (isFieldValidForRule rule) ticket

isFieldValidForRules rules x =
  any (`isFieldValidForRule` x) rules

isFieldValidForRule :: Rule -> Int -> Bool
isFieldValidForRule rule x =
  (a rule <= x && x <= b rule) || (c rule <= x && x <= d rule)

findInvalidFields :: ([Rule], [Int], [[Int]]) -> [Int]
findInvalidFields (rules, yours, nearby) =
  filter (not . isFieldValidForRules rules) (concat nearby)

validTickets rules = filter (isTicketValidForRules rules)

solve1 = sum . findInvalidFields

---

readInt x = read x :: Int

parseRule :: String -> Rule
parseRule s =
  Rule (head vals) (head bounds) (bounds !! 1) (bounds !! 2) (bounds !! 3)
  where
    result = s =~ "([a-z]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" :: MatchResult String
    vals = mrSubList result
    bounds = map readInt (drop 1 vals)

parseTicket s =
  map readInt (splitOn "," s)

parse ls =
  (map parseRule rules, parseTicket (head yours), map parseTicket nearby)
  where
    (rules:rest:_) = splitOn ["your ticket:"] $ filter (not . null) ls
    (yours:nearby:_) = splitOn ["nearby tickets:"] rest


td = parse ["class: 1-3 or 5-7",
            "row: 6-11 or 33-44",
            "seat: 13-40 or 45-50",
            "",
            "your ticket:",
            "7,1,14",
            "",
            "nearby tickets:",
            "7,3,47",
            "40,4,50",
            "55,2,20",
            "38,6,12"]

main = do
  dat <- readFile "data.txt"
  let input = parse $ lines dat

  print $ solve1 input
  -- 27870

  -- print $ solve1 input
