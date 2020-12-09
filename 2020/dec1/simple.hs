-- Trivial but quite slow solution with list comprehensions

solve1 m xs = head [ a * b | a <- xs, b <- xs, a + b == m]

solve2 m xs = head [ a * b * c
                 | a <- xs,
                   b <- xs,
                   c <- xs,
                   a + b + c == m]

main = do
  dat <- readFile "data.txt"
  let xs = [ read x :: Int | x <- lines dat ]

  print $ solve1 2020 xs
  print $ solve2 2020 xs
