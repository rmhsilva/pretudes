

data Position = Position Int Int
  deriving Show


charAt treesmap (Position x y) =
  row !! xwrap
  where
    xwrap = x `mod` length (head treesmap)
    row = treesmap !! y


isHit :: [String] -> Position -> Bool
isHit treesmap position =
  charAt treesmap position == '#'


dy = 1
dx = 3

step (Position x y) = Position (x + dx) (y + dy)


getAllSteps :: [String] -> [Position]
getAllSteps treesmap = zipWith Position [0,dx..] [0,dy..(height-1)]
  where
    height = length treesmap


walk :: [String] -> [Bool]
walk treesmap =
  map (isHit treesmap) (getAllSteps treesmap)


solve :: [String] -> Int
solve treesmap =
  sum $ map (\x -> if x then 1 else 0) $ walk treesmap


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
  print $ solve $ lines dat
  -- 262
