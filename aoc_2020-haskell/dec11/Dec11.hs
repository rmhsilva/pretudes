-- yaaas cellular automata
-- TODO check https://github.com/MatthiasCoppens/AOC2020/blob/master/day11/solution.hs

import Data.Maybe
import Data.Array
import Data.Foldable (toList)


-- naive chunk
chunks _ [] = []
chunks w xs = take w xs : chunks w (drop w xs)


type Cell = Char
type Position = (Int, Int)
type Configuration = Array Position Cell


width :: Configuration -> Int
width = (+1) . fst . snd . bounds

height :: Configuration -> Int
height = (+1) . snd . snd . bounds

showc :: Configuration -> IO ()
showc c = putStr $ unlines $ chunks (width c) $ toList c

at :: Configuration -> Position -> Cell
at c pos = c ! pos

maybeAt :: Configuration -> Position -> Maybe Cell
maybeAt c (x, y)
  | x < 0 || x >= width c || y < 0 || y >= height c = Nothing
  | otherwise = Just (c `at` (x, y))



sightlines
  :: (Num b, Num a, Enum b, Enum a) => a -> b -> a -> b -> [[(a, b)]]
sightlines w h x y =
  filter (not . null) [upleft,   up,   upright,
                       left,           right,
                       downleft, down, downright]
  where
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
  mapMaybe v' $ sightlines (width c) (height c) x y
  where
    maybeHead [] = Nothing
    maybeHead (x:_) = Just x
    v' :: [Position] -> Maybe Cell
    v' = maybeHead . filter (/='.') . map (c `at`)


adjacentCells :: Configuration -> Position -> [Cell]
adjacentCells c (x, y) = mapMaybe (maybeAt c) positions
  where
    positions = [(x-1, y-1), (x, y-1), (x+1, y-1),
                 (x-1, y),             (x+1, y),
                 (x-1, y+1), (x, y+1), (x+1, y+1)]

numOccupied :: [Cell] -> Int
numOccupied = length . filter (=='#')


nextState :: Int -> (Cell, Int) -> Cell
nextState tolerance ('L', 0) = '#'
nextState tolerance ('L', _) = 'L'
nextState tolerance ('#', x) = if x >= tolerance then 'L' else '#'
nextState tolerance (c, _)   = error $ show c


type NextCellFunction = Configuration -> Position -> Cell

nextCell :: Int -> (Configuration -> Position -> [Cell]) -> NextCellFunction
nextCell tolerance checkAt c pos =
  nextState tolerance (c `at` pos, numOccupied $ checkAt c pos)

nextCell1 :: NextCellFunction
nextCell1 = nextCell 4 adjacentCells

nextCell2 :: NextCellFunction
nextCell2 = nextCell 5 visibleCells


step :: NextCellFunction -> Configuration -> Configuration
step nextCellFn c =
  c // [ ((x, y), nextCellFn c (x, y))
       | y <- [0..height c - 1],
         x <- [0..width c - 1],
         c ! (x, y) /= '.'] -- small optimisation


countEq x = length . filter (==x)

solve :: NextCellFunction -> Configuration -> Int
solve stepFn c =
  countEq '#' $ toList $ iter' c (step stepFn c)
  where
    iter' previous current
      | previous == current = current
      | otherwise = iter' current (step stepFn current)


solve1 = solve nextCell1
solve2 = solve nextCell2

parse :: [String] -> Configuration
parse xs = listArray bounds (concat xs)
  where
    bounds = ((0,0),
              (length (head xs) - 1, length xs - 1))


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
             "..#L....#",  -- L is maybeAt (3,4)
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
