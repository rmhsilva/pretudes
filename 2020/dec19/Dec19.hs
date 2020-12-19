
module Dec19 where

import Data.List.Split (splitOn)
import Text.Regex.PCRE (mrSubList, MatchResult, (=~))
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
  deriving Show


type RuleMap = Map Int Rule


-- This is for parsing the messages

-- buggy = M.fromList [(0, Or (Terminal 'a') (Seq [Terminal 'a', Terminal 'b']))]
-- parse buggy "aba" $ ManySub 0

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
parse rules str     (ManySub  i) = case parseRepeated rules str (Sub i) of
                                     [] -> Nothing
                                     xs -> last xs

parseRepeated :: RuleMap -> String -> Rule -> [Maybe String]
parseRepeated rules s ruleStr = let result = parse rules s ruleStr in
                               case result of
                                 Nothing -> []
                                 Just s' -> result : parseRepeated rules s' ruleStr


--

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
  (ruleNumber, parseRule ruleStr)
  where
    result = s =~ "([0-9]+): (.+)$" :: MatchResult String
    matches = mrSubList result
    ruleNumber = read (head matches) :: Int
    ruleStr = matches !! 1


parseInput :: [String] -> (RuleMap, [String])
parseInput s =
  (M.fromList $ map parseLine rules, messages)
  where
    (rules:messages:_) = splitOn [""] s


patch (original, tests) = (M.insert 0 newZero original, tests)
  where
    newZero = Seq [Sub 42, ManySub 42, ManySub 31]  -- needs 2 or more 42s

parseInput2 = patch . parseInput


--

ruleZeroWithEof rules = case rules ! 0 of
                          Seq rs -> Seq $ rs ++ [Eof]
                          _ -> error "ruleStr 0 isn't a sequence"

matching tests rules = mapMaybe (\t -> (const t) <$> parse rules t startRule) tests
  where
    startRule = ruleZeroWithEof rules

solve (rules, tests) =
  length $ tests `matching` rules

check (rules, tests) =
  filter ((>0) . length) $ mapMaybe (\t -> parse rules t (rules ! 0)) tests


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
                  "0: 8 11",     -- original
                  -- "0: 42 11",       -- patch!
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
                  -- "11: 42+ 31+",              -- Many
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


-- 42 == bbabb | bbaab
-- 31 ==
-- parse rules2 "bbabbbbabb" (rules2 ! 8)  -- twice sequence of 42
--
-- test1: bbabbbbaabaabba
-- bbabb bbaab aabba
-- 42    42    31


{-

0: 1 eof
1: 2 | 2 1
2: a

matches:

a
aa
aaa
....

-}


main = do
  dat <- readFile "data.txt"
  let input = parseInput $ lines dat

  print $ solve input
  -- 222

  let input2 = parseInput2 $ lines dat

  print $ solve input2
  -- 244 is too low
  -- 348 is definitely wrong
  -- 347 is also wrong
  -- 349 is wrong too
  -- 352 is too high

  -- let (rules, tests) = input2
  -- mapM_ print $ M.toList rules
