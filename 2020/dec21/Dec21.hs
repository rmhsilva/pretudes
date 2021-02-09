
module Dec21 where

import Data.List (groupBy, sort, sortBy, intercalate)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Text.Regex.PCRE (mrSubList, MatchResult, (=~))


type Ingredient = String
type Allergen   = String


solve1 input =
  sum $ map (countIn $ unlines input) $ S.toList $ definitelyNot $ merge $ parse input

solve2 =
  intercalate "," . map snd . sortBy (compare `on` fst) . bla . narrow . merge . parse


narrow merged = zip (map fst merged) (map possibleAllergens merged)


bla :: (Ord i, Eq a) => [(a, Set i)] -> [(a, i)]
bla xs = fst $ until ((==length xs) . length . fst) pass ([], xs)

pass :: (Ord i, Eq a) => ([(a, i)], [(a, Set i)]) -> ([(a,i)], [(a, Set i)])
pass (alreadyKnown, xs) = (alreadyKnown', filter notFound xs)
  where
    alreadyKnown' = foldl bar alreadyKnown xs
    notFound x = fst x `notElem` map fst alreadyKnown'


bar :: (Ord i) => [(a, i)] -> (a, Set i) -> [(a, i)]
bar alreadyMatched (allergen, possibles) =
  case matchAllergen (S.fromList $ map snd alreadyMatched) possibles of
    Just ingredient  -> (allergen, ingredient) : alreadyMatched
    Nothing          -> alreadyMatched


matchAllergen :: (Ord i) => Set i -> Set i -> Maybe i
matchAllergen alreadyKnown possibles
 | length possibles == 1 = Just $ head (S.toList possibles)
 | otherwise = let diff = S.toList $ S.difference possibles alreadyKnown in
                 case diff of
                   [x] -> Just x
                   _:_ -> Nothing


-- count times a word appears in a sentence
countIn :: String -> String -> Int
countIn s x = length $ filter (== x) $ words s

definitelyAllergens = foldl1 S.union . map possibleAllergens

definitelyNot xs    = S.difference (allIngedients xs) (definitelyAllergens xs)

allIngedients :: [(Allergen, [Set Ingredient])] -> Set Ingredient
allIngedients  = foldl1 S.union . concatMap snd

possibleAllergens :: (Allergen, [Set Ingredient]) -> Set Ingredient
possibleAllergens  = foldl1 S.intersection . snd

merge :: (Ord a, Ord b) => [(a, [b])] -> [(a, [Set b])]
merge =
  map (\xs -> (fst $ head xs, map (S.fromList . snd) xs))
  . groupBy ((==) `on` fst)
  . sortBy (compare `on` fst)


findAssocs :: String -> [(Allergen, [String])]
findAssocs l = [(a, ingredients) | a <- allergens]
  where
    matches = mrSubList (l =~ "(.*) \\(contains (.*)\\)" :: MatchResult String)
    ingredients = words (head matches)
    allergens = splitOn ", " (matches !! 1)


parse :: [String] ->[(String, [String])]
parse = concatMap findAssocs


td = ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
      "trh fvjkl sbzzf mxmxvkd (contains dairy)",
      "sqjhc fvjkl (contains soy)",
      "sqjhc mxmxvkd sbzzf (contains fish)"]

merged = (merge . parse) td


main = do
  dat <- readFile "data.txt"
  let input = lines dat

  print $ solve1 input
  print $ solve2 input
