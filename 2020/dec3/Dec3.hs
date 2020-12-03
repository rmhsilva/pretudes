-- Yay!

data Position = Position Int Int
  deriving Show

data Slope = Slope Int Int
  deriving Show


charAt treesmap (Position x y) =
  row !! xwrap
  where
    xwrap = x `mod` length (head treesmap)
    row = treesmap !! y


isHit :: [String] -> Position -> Bool
isHit treesmap position =
  charAt treesmap position == '#'


-- Get all steps (positions) when walking over a given treemap with slope
getAllSteps :: Slope -> [String] -> [Position]
getAllSteps (Slope dx dy) treesmap =
  zipWith Position [0,dx..] [0,dy..(height-1)]
  where
    height = length treesmap


walk :: Slope -> [String] -> [Bool]
walk slope treesmap =
  map (isHit treesmap) (getAllSteps slope treesmap)


solve1 :: [String] -> Slope -> Int
solve1 treesmap slope =
  sum $ map (\x -> if x then 1 else 0) $ walk slope treesmap

solve2 treesmap =
  product hits
  where
    slopes = [Slope 1 1,
              Slope 3 1,
              Slope 5 1,
              Slope 7 1,
              Slope 1 2]
    hits = map (solve1 treesmap) slopes


td :: [String]
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
