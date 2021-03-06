--
module Dec16 where

import Data.List.Split (splitOn)
import Text.Regex.PCRE (mrSubList, MatchResult, (=~))
import Data.List (groupBy, sort, sortBy)
import Data.Function (on)


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

solve1 = sum . findInvalidFields


---

nthField tickets n = map (!!n) tickets

nthFields tickets = map (nthField tickets) [0..length (head tickets) - 1]


allFieldsValidForRule :: [Int] -> Rule -> Bool
allFieldsValidForRule xs rule = all (isFieldValidForRule rule) xs

validTickets rules = filter (isTicketValidForRules rules)



-- problem: some rules match more than one group. Some groups match more than
-- one rule.
--
-- Find triples of (rule, group, index) such that
-- (allFieldsValidForRule rule group) is True


ruleGroupPairs (rules, yours, nearby) =
  [ (name rule, snd groupAndIndex) |
    rule <- rules,
    groupAndIndex <- zip (nthFields (validTickets rules nearby)) [0..],
    allFieldsValidForRule (fst groupAndIndex) rule
  ]


groupedPairs =
  sortBy (compare `on` length) . groupBy (\a b -> fst a == fst b) . ruleGroupPairs


validIndex xs acc = head $ filter (not . (`elem` acc)) xs


recur :: [(String, Int)] -> [[(String, Int)]] -> [(String, Int)]
recur acc [] = acc
recur acc (this:rest) =
  recur ((thisField, thisPosition) : acc) rest
  where
    thisField = fst (head this)
    thisPosition = validIndex (map snd this) (map snd acc)


fieldIndices = recur [] . groupedPairs


solve2' input@(rules, yours, nearby) = product $ map (yours!!) indices
  where
    indices = map snd $ filter (\f -> take 9 (fst f) == "departure" ) $ fieldIndices input



---

readInt x = read x :: Int

parseRule :: String -> Rule
parseRule s =
  Rule (head vals) (head bounds) (bounds !! 1) (bounds !! 2) (bounds !! 3)
  where
    result = s =~ "([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" :: MatchResult String
    vals = mrSubList result
    bounds = map readInt (drop 1 vals)

parseTicket s =
  map readInt (splitOn "," s)

parse ls =
  (map parseRule rules, parseTicket (head yours), map parseTicket nearby)
  where
    (rules:rest:_) = splitOn ["your ticket:"] $ filter (not . null) ls
    (yours:nearby:_) = splitOn ["nearby tickets:"] rest


td = parse ["class: 1-3 or 5-7", "row: 6-11 or 33-44", "seat: 13-40 or 45-50",
            "",
            "your ticket:", "7,1,14",
            "",
            "nearby tickets:", "7,3,47", "40,4,50", "55,2,20", "38,6,12"]


td2 = parse ["class: 0-1 or 4-19", "row: 0-5 or 8-19", "seat: 0-13 or 16-19",
             "your ticket:", "11,12,13",
             "nearby tickets:", "3,9,18", "15,1,5", "5,14,9"]

main = do
  dat <- readFile "data.txt"
  let input = parse $ lines dat

  print $ solve1 input
  -- 27870

  print $ solve2' input
  -- 3173135507987
