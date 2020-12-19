-- Oops. Tried to be clever and implement a full parser engine.
--
--
-- Wasted too long thinking that
--   11: 42 31 | 42 11 31  === 11: 42+ 31+
--
-- It's not. The number of 42s and 31s must match. The RHS doesn't guarantee
-- that. This is true:
--
--   11: 42 31 | 42 11 31  === 11: 42 11? 31
--
-- But building my part1 parser is greedy, and the 42+ in rule8 chomps the
-- initial 42 in this nice rule11.
--
-- Copped out in the end and just converted it to a recursive regex. Meh.


module Dec19 where

import Data.List.Split (splitOn)
import Text.Regex.PCRE (mrSubList, mrMatch, MatchResult, (=~))
import Control.Monad
import Data.Maybe
import Text.Read (readMaybe)
import qualified Data.Map as M
import Data.Map (Map, (!))


data Rule = Eof             -- END (special)
          | Terminal Char   -- "x"
          | Sub Int         -- a
          | Seq [Rule]      -- a b
          | Or Rule Rule    -- a | b
          | ManySub Int     -- a+
          | MaybeSub Int    -- a?
          | Rule11          -- urgh, copout for part 2
  deriving Show


type RuleMap = Map Int Rule


-- This is for parsing the messages

parse :: RuleMap -> String -> Rule -> Maybe String
parse rules ""       Eof         = Just ""
parse rules str      Eof         = Nothing
parse rules ""      (Terminal c) = Nothing
parse rules (x:str) (Terminal c) = if x == c then Just str else Nothing
parse rules str     (Sub i     ) = parse rules str (rules ! i)
parse rules str     (Seq rs    ) = foldM (parse rules) str rs
parse rules str     (Or  ra rb ) = case parse rules str ra of
                                     Just str' -> Just str'
                                     Nothing   -> parse rules str rb
-- for part 2:
parse rules str     (MaybeSub i) = case parse rules str (Sub i) of
                                     Just s  -> Just s
                                     Nothing -> Just str
parse rules str     (ManySub  i) = case parseRepeated rules str (Sub i) of
                                     [] -> Nothing
                                     xs -> last xs

parseRepeated :: RuleMap -> String -> Rule -> [Maybe String]
parseRepeated rules s ruleStr = let result = parse rules s ruleStr in
                               case result of
                                 Nothing -> []
                                 Just s' -> result : parseRepeated rules s' ruleStr

--
-- just convert it to a regex...

toRegex :: RuleMap -> Rule -> String
toRegex rules (Terminal c)  = [c]
toRegex rules (Sub i     )  = toRegex rules $ rules ! i
toRegex rules (Seq rs    )  = concat $ map (toRegex rules) rs
toRegex rules (Or a b    )  = "(?:" ++ (toRegex rules a) ++ "|" ++ (toRegex rules b) ++ ")"
toRegex rules (ManySub i )  = "(?:" ++ (toRegex rules $ rules ! i) ++ ")" ++ "+"
toRegex rules  Rule11       = "(" ++ rule42 ++ "(?1)?" ++ rule31 ++ ")"
  where
    rule42 = toRegex rules (rules ! 42)
    rule31 = toRegex rules (rules ! 31)


-- This is for parsing the input rules

parseTerminal, parseSub, parseOr, parseSeq :: String -> Maybe Rule

parseTerminal s = case ( readMaybe s :: Maybe String ) of
                    Just s' -> Just $ Terminal (head s')
                    Nothing -> Nothing

parseSub s =
  if ' ' `elem` s then Nothing
  else Sub <$> (readMaybe (head matches) :: Maybe Int)
  where
    matches = mrSubList (s =~ "([0-9]+)" :: MatchResult String)

parseManySub s =
  if not ('+' `elem` s) then Nothing
  else case parseSub (take (length s - 1) s) of
         Just (Sub x) -> Just $ ManySub x
         Nothing      -> Nothing

parseOr s =
  if not ('|' `elem` s) then Nothing
  else Just $ Or (parseRule left) (parseRule right)
  where
    (left:right:_) = splitOn " | " s

parseSeq s = case map parseRule $ splitOn " " s of
               [] -> Nothing
               xs -> Just $ Seq xs

parseRule :: String -> Rule
parseRule s = head $ mapMaybe ($ s) parsers
  where
    parsers = [parseTerminal, parseManySub, parseSub, parseOr, parseSeq]


parseLine :: String -> (Int, Rule)
parseLine s =
  (ruleNumber, rule)
  where
    result = s =~ "([0-9]+): (.+)$" :: MatchResult String
    matches = mrSubList result
    ruleNumber = read (matches !! 0) :: Int
    rule = parseRule (matches !! 1)


parseInput :: [String] -> (RuleMap, [String])
parseInput s =
  (M.fromList $ map parseLine rules, messages)
  where
    (rules:messages:_) = splitOn [""] s


patch (original, tests) = (M.union new original, tests)
  where
    new = M.fromList [ (8,  ManySub 42), -- bug: greedy
                       (11, Seq [Sub 42, MaybeSub 11, Sub 31])]

patch' (original, tests) = (M.union new original, tests)
  where
    new = M.fromList [ (8,  ManySub 42),
                       (11, Rule11) ]

parseInput2 = patch' . parseInput


--

ruleZeroWithEof rules = case rules ! 0 of
                          Seq rs -> Seq $ rs ++ [Eof]
                          _ -> error "rule 0 isn't a sequence"

matching tests rules = mapMaybe (\t -> (const t) <$> parse rules t startRule) tests
  where
    startRule = ruleZeroWithEof rules

solve (rules, tests) =
  length $ tests `matching` rules


matches :: RuleMap -> String -> Bool
matches rules s =
  length match == length s
  where
    reg = toRegex rules (rules ! 0)
    match = mrMatch (s =~ (reg ++ "$") :: MatchResult String)

matching' tests rules =
  filter id $ map (matches rules) tests

solve' (rules, tests) =
  length $ tests `matching'` rules

--

td = parseInput ["0: 4 1 5",
                 "1: 2 3 | 3 2",
                 "2: 4 4 | 5 5",
                 "3: 4 5 | 5 4",
                 "4: \"a\"",
                 "5: \"b\"",
                 "",
                 "ababbb",
                 "bababa",
                 "abbbab",
                 "aaabbb",
                 "aaaabbb"]

td2 = parseInput2 [
                  "0: 8 11",
                  "1: \"a\"",
                  "2: 1 24 | 14 4",
                  "3: 5 14 | 16 1",
                  "4: 1 1",
                  "5: 1 14 | 15 1",
                  "6: 14 14 | 1 14",
                  "7: 14 5 | 1 21",
                  "8: 42",              -- original
                  -- "8: 42 | 42 8",       -- LOOP => 42 [42 42 42 ...]
                  -- "8: 42+",             -- Many
                  "9: 14 27 | 1 26",
                  "10: 23 14 | 28 1",
                  "11: 42 31",             -- original
                  -- "11: 42 31 | 42 11 31",  -- LOOP => 42 [42 42 ... 31 31] 31
                  "12: 24 14 | 19 1",
                  "13: 14 3 | 1 12",
                  "14: \"b\"",
                  "15: 1 | 14",
                  "16: 15 1 | 14 14",
                  "17: 14 2 | 1 7",
                  "18: 15 15",
                  "19: 14 1 | 14 14",
                  "20: 14 14 | 1 15",
                  "21: 14 1 | 1 14",
                  "22: 14 14",
                  "23: 25 1 | 22 14",
                  "24: 14 1",
                  "25: 1 1 | 1 14",
                  "26: 14 22 | 1 20",
                  "27: 1 6 | 14 18",
                  "28: 16 1",
                  "31: 14 17 | 1 13",
                  "42: 9 14 | 10 1",
                  "",
                  "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
                  "bbabbbbaabaabba",
                  "babbbbaabbbbbabbbbbbaabaaabaaa",
                  "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
                  "bbbbbbbaaaabbbbaaabbabaaa",
                  "bbbababbbbaaaaaaaabbababaaababaabab",
                  "ababaaaaaabaaab",
                  "ababaaaaabbbaba",
                  "baabbaaaabbaaaababbaababb",
                  "abbbbabbbbaaaababbbbbbaaaababb",
                  "aaaaabbaabaaaaababaa",
                  "aaaabbaaaabbaaa",
                  "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
                  "babaaabbbaaabaababbaabababaaab",
                  "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba" ]  -- 12

rules2 = fst td2
tests2 = snd td2


main = do
  dat <- readFile "data.txt"
  let input = parseInput $ lines dat

  print $ solve input
  -- 222

  let input2 = parseInput2 $ lines dat

  print $ solve' input2
  -- 339

  -- let (rules, tests) = input2
  -- mapM_ print $ M.toList rules
