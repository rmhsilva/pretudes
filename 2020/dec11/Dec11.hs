-- yaaas cellular automata

import Data.Maybe
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq)
import Data.Foldable (toList)

type Cell = Char
type Position = (Int, Int)
type Configuration = Seq (Seq Cell)


width :: Configuration -> Int
width c = Sequence.length (c `Sequence.index` 0)

height :: Configuration -> Int
height = Sequence.length

showc :: Configuration -> IO ()
showc c = putStr $ concat $ foldRows c
  where
    foldRows = fmap ((++"\n") . toList)


at :: Configuration -> Position -> Maybe Cell
at c (x, y)
  | x < 0 || x >= width c || y < 0 || y >= height c = Nothing
  | otherwise = Just $ (c `Sequence.index` y) `Sequence.index` x


updateAt :: Configuration -> Position -> Cell -> Configuration
updateAt c (x, y) v =
  Sequence.update y (Sequence.update x v (c `Sequence.index` y)) c


visibleCells :: Configuration -> Position -> [Cell]
visibleCells c (x, y) = []

adjacentCells :: Configuration -> Position -> [Cell]
adjacentCells c (x, y) = mapMaybe (at c) positions
  where
    positions = [(x-1, y-1), (x, y-1), (x+1, y-1),
                 (x-1, y),             (x+1, y),
                 (x-1, y+1), (x, y+1), (x+1, y+1)]

numOccupied :: [Cell] -> Int
numOccupied = length . filter (=='#')

numAdjacentOccupied :: Configuration -> Position -> Int
numAdjacentOccupied c = numOccupied . adjacentCells c

numVisibleOccupied :: Configuration -> Position -> Int
numVisibleOccupied c = numOccupied . visibleCells c


nextState :: (Cell, Int) -> Cell
nextState ('L', 0) = '#'
nextState ('#', x) = if x >= 4 then 'L' else '#'
nextState (c, _)   = c


nextCell :: Configuration -> Position -> Cell
nextCell c pos = nextState (fromJust (c `at` pos), numAdjacentOccupied c pos)

cmap :: (Configuration -> Position -> a) -> Configuration -> [a]
cmap f c = [f c (x, y) |
             y <- [0..height c - 1],
             x <- [0..width c - 1]]

step :: Configuration -> Configuration
step c =
  fromList (width c) $ cmap nextCell c


count x = length . filter (==x)

solve1 :: Configuration -> Int
solve1 c = count True $ cmap (\c pos -> (c `at` pos) == Just '#') $ solve1' c (step c)
  where
    solve1' previous current
      | previous == current = current
      | otherwise = solve1' current (step current)


-- naive chunk
chunks _ [] = []
chunks w xs = take w xs : chunks w (drop w xs)

fromList :: Int -> [Cell] -> Configuration
fromList w xs = Sequence.fromList $ map Sequence.fromList (chunks w xs)


parse :: [String] -> Configuration
parse xs = fromList (length (head xs)) $ concat xs


td :: Configuration
td = parse ["L.LL.LL.LL",
            "LLLLLLL.LL",
            "L.L.L..L..",
            "LLLL.LL.LL",
            "L.LL.LL.LL",
            "L.LLLLL.LL",
            "..L.L.....",
            "LLLLLLLLLL",
            "L.LLLLLL.L",
            "L.LLLLL.LL"]


main = do
  dat <- readFile "data.txt"
  let input = parse $ lines dat

  print $ solve1 input
  -- 2361 (20 seconds using the terrible Seq method)

  -- print $ solve2 input
