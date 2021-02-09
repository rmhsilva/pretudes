{-

There are 3 axes. (3^3) - 1 neighbours = 26.

26 different ways to shift the focus. 26 directions.

The universe is

How do you define a lazily evaluated "path" for exploring outwards from a given
point in a space, such that you explore the closest points first?

In 1-D, it's easy: two linked lists.

In 2-D?

Partition the space along a line. Map each half of the space onto a single
dimension, and then walk that?

Map it onto a circle centered at the point? r = x^2 + y^2. x=[0..], x=[0..]

And then, how do you shift it the focal point?

In 1-D, it's easy: shift it left or right.

In 2-D?

Shift it in eight directions.

Um.
https://chrispenner.ca/posts/conways-game-of-life

-}

-- {-# language GeneralizedNewtypeDeriving #-}
-- {-# language TypeFamilies #-}
module Dec17 where

import Data.Functor.Compose (Compose(..))
-- import qualified Data.Vector as V
-- import Data.Bool (bool)
-- import Data.Distributive (Distributive(..))
-- import Data.Functor.Rep (Representable(..), distributeRep)
-- import Data.Functor.Identity (Identity(..))
-- import Control.Arrow ((***))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad (Comonad(..))


newtype VBounded a = VBounded (V.Vector a)
  deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
  distribute = distributeRep

gridSize :: Int
gridSize = 20

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v V.! (i `mod` gridSize)
  tabulate desc = VBounded $ V.generate gridSize desc


type Space a = Store (Compose VBounded VBounded VBounded) a
type Coord = (Int, Int, Int)

mkGrid :: [Coord] -> Space Bool
mkGrid xs = store lookup (0,0,0)
  where
    lookup crd = crd `elem` xs


-- unit vectors around a point
directions = map tup $ filter (/= [0,0,0]) $ replicateM 3 [-1,0,1]
  where
    tup (a:b:c:_) = (a,b,c)


type Rule = Space Bool -> Bool

basicRule :: Rule
basicRule g =
  (alive && numNeighboursAlive `elem` [2, 3]) || (not alive && numNeighboursAlive == 3)
  where
    alive = extract g
    addCoords (x,y,z) (x',y',z') = (x+x', y+y', z+z')
    neighbours = experiment (\s -> addCoords s <$> directions) g
    numNeighboursAlive = length (filter id neighbours)


step :: Rule -> Space Bool -> Space Bool
step = extend
