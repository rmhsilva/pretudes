-- Fun with floating point precision! Lost too much time over a typo in the
-- rotation maths.
module Dec12 where

import Data.Fixed (mod')

type Instruction = (Char, Double)

data Ship = Ship { posX :: Double, posY :: Double, angle_deg :: Double }
  deriving Show

-- Part 2 ship is a vector
data Ship2 = Ship2 { posX' :: Double, posY' :: Double, wayX :: Double, wayY :: Double }
  deriving Show


initialShip = Ship 0 0 0
initialShip2 = Ship2 0 0 10 1

-- angle in radians
angle :: Ship -> Double
angle s = pi * angle_deg s / 180

-- like signum, but treats 0 as positive
sign x | x < 0     = -1
       | otherwise = 1

-- part 1
navigate :: Ship -> Instruction -> Ship
navigate s ('N', d) = s { posY = posY s + d}
navigate s ('S', d) = s { posY = posY s - d}
navigate s ('E', d) = s { posX = posX s + d}
navigate s ('W', d) = s { posX = posX s - d}
navigate s ('L', a) = s { angle_deg = (angle_deg s + a) `mod'` 360}
navigate s ('R', a) = s { angle_deg = result `mod'` (sign result * 360)}
  where
    result = angle_deg s - a
navigate s ('F', d) = s { posX = posX s + d * cos ( angle s ),
                          posY = posY s + d * sin ( angle s )}

-- part 2
navigate2 :: Ship2 -> Instruction -> Ship2
navigate2 s ('N', d) = s { wayY = wayY s + d}
navigate2 s ('S', d) = s { wayY = wayY s - d}
navigate2 s ('E', d) = s { wayX = wayX s + d}
navigate2 s ('W', d) = s { wayX = wayX s - d}
navigate2 s ('L', a) = rotateWaypoint s a
navigate2 s ('R', a) = rotateWaypoint s (-1 * a)
navigate2 s ('F', n) =
  s { posX' = posX' s + dx, posY' = posY' s + dy }
  where
    dx = n * wayX s
    dy = n * wayY s


-- Haskell's numbers aren't precise enough for part 2 - the errors accumulate
-- way too much. But all of our instructions contain "simple" angles

sin' a | a < 0    = -1 * sin' (abs a)
       | a == 90  = 1
       | a == 180 = 0
       | a == 270 = -1
       | otherwise = error $ "sin' " ++ (show a)

cos' a | a < 0    = cos' (abs a)
       | a == 90  = 0
       | a == 180 = -1
       | a == 270 = 0
       | otherwise = error $ "cos' " ++ (show a)

rotateWaypoint s deg =
  s { wayX = x', wayY = y' }
  where
    -- Rx = [ cos a  -sin a ] . [ x ]
    --      [ sin a   cos a ]   [ y ]
    --
    --    = [ x cos a - y sin a ] = [ x' ]
    --      [ x sin a + y cos a ] = [ y' ]
    x = wayX s
    y = wayY s
    x' = (x * cos' deg) - (y * sin' deg)
    y' = (x * sin' deg) + (y * cos' deg)


---

solve1 xs = abs (posX ship) + abs (posY ship)
  where
    ship = foldl navigate initialShip xs

solve2 xs = abs (posX' ship) + abs (posY' ship)
  where
    ship = foldl navigate2 initialShip2 xs


parse :: String -> Instruction
parse s = (head s, read (drop 1 s) :: Double)

td :: [Instruction]
td = map parse ["F10", "N3", "F7", "R90", "F11"]

main = do
  dat <- readFile "data.txt"
  let input = map parse $ lines dat

  print $ solve1 input
  -- 882

  print $ solve2 input
  -- 28885
