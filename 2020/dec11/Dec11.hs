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


sightlines w h x y =
  filter (not . null) [upleft,   up,   upright,
                       left,           right,
                       downleft, down, downright]
  where
    -- :: [[(x, y)]]
    left       = zip [x-1,x-2..0] (repeat y)
    right      = zip [x+1..w-1]   (repeat y)
    up         = zip (repeat x)   [y-1,y-2..0]
    down       = zip (repeat x)   [y+1..h-1]
    upleft     = zip [x-1,x-2..0] [y-1,y-2..0]
    upright    = zip [x+1..w-1]   [y-1,y-2..0]
    downleft   = zip [x-1,x-2..0] [y+1..h-1]
    downright  = zip [x+1..w-1]   [y+1..h-1]


visibleCells :: Configuration -> Position -> [Cell]
visibleCells c (x, y) =
  catMaybes $
  map v' $ sightlines (width c) (height c) x y
  where
    maybeHead [] = Nothing
    maybeHead (x:_) = Just x
    v' :: [Position] -> Maybe Cell
    v' = maybeHead . filter (/='.') . mapMaybe (c `at`)


adjacentCells :: Configuration -> Position -> [Cell]
adjacentCells c (x, y) = mapMaybe (at c) positions
  where
    positions = [(x-1, y-1), (x, y-1), (x+1, y-1),
                 (x-1, y),             (x+1, y),
                 (x-1, y+1), (x, y+1), (x+1, y+1)]

numOccupied :: [Cell] -> Int
numOccupied = length . filter (=='#')

type OccupiedCounter = Configuration -> Position -> Int

numAdjacentOccupied :: OccupiedCounter
numAdjacentOccupied c = numOccupied . adjacentCells c

numVisibleOccupied :: OccupiedCounter
numVisibleOccupied c = numOccupied . visibleCells c


nextState :: Int -> (Cell, Int) -> Cell
nextState tolerance ('L', 0) = '#'
nextState tolerance ('#', x) = if x >= tolerance then 'L' else '#'
nextState tolerance (c, _)   = c


type NextCellFunction = Configuration -> Position -> Cell

nextCell :: Int -> OccupiedCounter -> NextCellFunction
nextCell tolerance f c pos =
  nextState tolerance (fromJust (c `at` pos), f c pos)

nextCell1 :: NextCellFunction
nextCell1 = nextCell 4 numAdjacentOccupied

nextCell2 :: NextCellFunction
nextCell2 = nextCell 5 numVisibleOccupied


cmap :: (Configuration -> Position -> a) -> Configuration -> [a]
cmap f c = [f c (x, y) |
             y <- [0..height c - 1],
             x <- [0..width c - 1]]

step :: NextCellFunction -> Configuration -> Configuration
step nextCellFn c =
  fromList (width c) $ cmap nextCellFn c


count x = length . filter (==x)

solve :: NextCellFunction -> Configuration -> Int
solve stepFn c =
  count True $ cmap (\c pos -> (c `at` pos) == Just '#') $ solve1' c (step stepFn c)
  where
    solve1' previous current
      | previous == current = current
      | otherwise = solve1' current (step stepFn current)


solve1 = solve nextCell1
solve2 = solve nextCell2


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


td2 = parse [".......#.",
             "...#.....",
             ".#.......",
             ".........",
             "..#L....#",  -- L is at (3,4)
             "....#....",
             ".........",
             "#........",
             "...#....."]


main = do
  dat <- readFile "data.txt"
  let input = parse $ lines dat

  print $ solve1 input
  -- 2361 (~15 seconds using the terrible Seq method)

  print $ solve2 input
  -- 2119 (~20s with Seq)
