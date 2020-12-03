-- Yay!

data Position = Position Int Int
  deriving Show

data Slope = Slope Int Int
  deriving Show

type Treesmap = [String]


-- Get char at position in treemap
charAt :: Treesmap -> Position -> Char
charAt treesmap (Position x y) =
  row !! xwrap
  where
    xwrap = x `mod` length (head treesmap)
    row = treesmap !! y


-- Check if position is a tree
isHit :: Treesmap -> Position -> Bool
isHit treesmap position =
  charAt treesmap position == '#'


-- Get all steps (positions) when walking over a given treemap with slope
getAllSteps :: Slope -> Treesmap -> [Position]
getAllSteps (Slope dx dy) treesmap =
  zipWith Position [0,dx..] [0,dy..(height-1)]
  where
    height = length treesmap


-- Goooo down the slope, returning list of whether each step is a tree
gooo :: Slope -> Treesmap -> [Bool]
gooo slope treesmap =
  map (isHit treesmap) (getAllSteps slope treesmap)


-- Count number of trees
solve1 :: Treesmap -> Slope -> Int
solve1 treesmap slope =
  sum $ map (\x -> if x then 1 else 0) $ gooo slope treesmap


-- Do the same, multiple times...
solve2 treesmap =
  product hits
  where
    slopes = [Slope 1 1,
              Slope 3 1,
              Slope 5 1,
              Slope 7 1,
              Slope 1 2]
    hits = map (solve1 treesmap) slopes


td :: Treesmap
td = ["..##.......",
      "#...#...#..",
      ".#....#..#.",
      "..#.#...#.#",
      ".#...##..#.",
      "..#.##.....",
      ".#.#.#....#",
      ".#........#",
      "#.##...#...",
      "#...##....#",
      ".#..#...#.#"] -- 11 lines (max y = 10)

main = do
  dat <- readFile "data.txt"
  putStrLn $ unlines $ take 5 $ lines dat

  print $ solve1 (lines dat) (Slope 3 1)
  -- 262

  print $ solve2 $ lines dat
  -- 2698900776
