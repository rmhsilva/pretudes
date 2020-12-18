-- cellular automata are comonads...
-- http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html


-- a universe is a point with some neighbours
data U x = U [x] x [x]

-- the focal point can be shifted
right (U a b (c:cs)) = U (b:a) c cs
left  (U (a:as) b c) = U as a (b:c)


-- and it can be trivially mapped over
instance Functor U where
   fmap f (U a b c) = U (map f a) (f b) (map f c)


-- comonads are the 'dual' of monads
class Functor w => Comonad w where
  (=>>)    :: w a -> (w a -> b) -> w b
  coreturn :: w a -> a
  cojoin   :: w a -> w (w a)
  x =>> f   = fmap f (cojoin x)

-- (=>>) applies f to the universe of universes. What is that?! Well... Look
-- closely at cojoin. It turns `a` into a 'universe' of 'universes' where each
-- element is a copy of `a` shifted left or right a number of times.
instance Comonad U where
  coreturn (U _ b _) = b
  cojoin a           = U (tail $ iterate left a) a (tail $ iterate right a)


-- here is a rule to update the focal point in a boolean universe
rule :: U Bool -> Bool
rule (U (a:_) b (c:_)) = not (a && b && not c || (a==b))


-- let's use a general shift
shift i u = iterate (if i<0 then left else right) u !! abs i

-- to read the universe into a list
toList i j u = take (j-i) $ half $ shift i u where
  half (U _ b c) = b:c


-- and add some IO to make it real
run n = let u = U (repeat False) True (repeat False)
        in putStr $
           unlines $
           take n $
           map (map (\x -> if x then '#' else ' ') . toList (-n) n) $
           iterate (=>> rule) u

test = run 30
