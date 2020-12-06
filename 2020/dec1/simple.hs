-- Trivial but quite slow solution with list comprehensions

main = do
  dat <- readFile "data.txt"
  let xs = [ read x :: Int | x <- lines dat ]
  print $ head [ a * b | a <- xs, b <- xs, a + b == 2020]
  print $ head [ a * b * c | a <- xs, b <- xs, c <- xs, a + b + c == 2020]
