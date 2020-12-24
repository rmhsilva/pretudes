module Dec22 where

import Debug.Trace
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Set as S
import Control.Monad.ST (ST, runST)
import qualified Control.Monad.Loops as Loop
import Data.STRef (newSTRef, writeSTRef, readSTRef, STRef)
import qualified Data.HashTable.ST.Cuckoo as Cuckoo
import qualified Data.HashTable.Class as H

type Deck = [Int]

solve1 = calculateWinnerScore . winnerCards . until finished play


calculateWinnerScore = sum . zipWith (*) [1..] . reverse

finished :: (Deck,Deck) -> Bool
finished (p1,p2) = null p1 || null p2

play :: (Deck,Deck) -> (Deck,Deck)
play (p1:p1s, p2:p2s) | p1 > p2   = (p1s ++ [p1, p2], p2s            )
                      | otherwise = (p1s            , p2s ++ [p2, p1])

winnerCards :: (Deck,Deck) -> Deck
winnerCards (p1,p2) | null p2   = p1
                    | otherwise = p2

--

solve2 = calculateWinnerScore . winningDeck . fightToTheEnd . newGame

type Game = (S.Set (Deck,Deck), Deck, Deck)

fightToTheEnd = until weHaveAWinner combat
weHaveAWinner = isJust . winnerOf

newGame :: (Deck,Deck) -> Game
newGame (p1s,p2s) = (S.empty, p1s', p2s)
  where
    -- p1s' = trace ("->: " ++ (show p1s) ++" "++ (show p2s)) p1s
    p1s' = p1s

winningDeck :: Game -> Deck
winningDeck game@(_,p1s,p2s)
  | winnerOf game == Just 1 = p1s
  | winnerOf game == Just 2 = p2s
  | otherwise               = error "No winner yet"

winnerOf :: Game -> Maybe Int
winnerOf (prev,p1,p2)
  | S.member (p1,p2) prev = Just 1
  | null p2            = Just 1
  | null p1            = Just 2
  | otherwise          = Nothing

combat :: Game -> Game
combat (prev, p1 : p1s, p2 : p2s)
  | length p1s >= p1 && length p2s >= p2 =
    win $ fromJust $ winnerOf $ fightToTheEnd $ newGame (take p1 p1s, take p2 p2s)
  | p1 > p2   = win 1
  | otherwise = win 2
  where
      win 1 = (S.insert (p1:p1s,p2:p2s) prev, p1s ++ [p1, p2], p2s            )
      win 2 = (S.insert (p1:p1s,p2:p2s) prev, p1s            , p2s ++ [p2, p1])

-- Originall I kept a Set of (p1++p2), which doesn't work because it discards
-- the boundary between the two decks, which IS important.


-- ST version is actually slower!

solve2ST = calculateWinnerScore . fst . fightToTheEndST

type HashTable s k v = Cuckoo.HashTable s k v
type GameST s = (HashTable s (Deck,Deck) Bool, STRef s Deck, STRef s Deck)

newTable :: ST s (HashTable s (Deck,Deck) Bool)
newTable = do H.new

makeGame :: HashTable s (Deck,Deck) Bool -> STRef s Deck -> STRef s Deck -> ST s (GameST s)
makeGame prev p1ref p2ref = do return (prev, p1ref, p2ref)

fightToTheEndST :: (Deck,Deck) -> (Deck,Int) -- winningDeck, winningPlayer
fightToTheEndST (p1,p2) = runST $ do
  prev  <- newTable
  p1ref <- newSTRef p1
  p2ref <- newSTRef p2
  let game = makeGame prev p1ref p2ref

  Loop.untilM_ (playST game) (gotAWinnerST game)

  winner <- winnerOfST game
  p1 <- readSTRef p1ref
  p2 <- readSTRef p2ref
  let winningDeck = if winner == Just 1 then p1 else p2
  return (winningDeck, fromJust winner)


gotAWinnerST :: ST s (GameST s) -> ST s Bool
gotAWinnerST g = do
  winner <- winnerOfST g
  return $ isJust winner

winnerOfST :: ST s (GameST s) -> ST s (Maybe Int)
winnerOfST g = do
  (prev, p1ref, p2ref) <- g
  p1 <- readSTRef p1ref
  p2 <- readSTRef p2ref
  inPrev <- H.lookup prev (p1,p2)
  return $ if ( isJust inPrev || null p2 ) then Just 1
           else if null p1 then Just 2
                else Nothing

playST :: ST s (GameST s) -> ST s (GameST s)
playST g = do
  (prev, p1ref, p2ref) <- g
  p1 <- readSTRef p1ref
  p2 <- readSTRef p2ref
  H.insert prev (p1,p2) True
  let (p1',p2') = playSTPure p1 p2
  writeSTRef p1ref p1'
  writeSTRef p2ref p2'
  return (prev,p1ref,p2ref)

playSTPure :: Deck -> Deck -> (Deck,Deck)
playSTPure (p1:p1s) (p2:p2s)
  | length p1s >= p1 && length p2s >= p2 =
    win $ snd $ fightToTheEndST (take p1 p1s, take p2 p2s)
  | p1 > p2                              = win 1
  | otherwise                            = win 2
  where
    win 1 = (p1s ++ [p1, p2], p2s            )
    win 2 = (p1s            , p2s ++ [p2, p1])


--

readInt x = read x :: Int

parse input = (map readInt (drop 1 p1s), map readInt (drop 1 p2s))
  where
    (p1s:p2s:_) = splitOn [""] input

td = parse ["Player 1:", "9", "2", "6", "3", "1",
            "",
            "Player 2:", "5", "8", "4", "7", "10"]

tdg = newGame td

infiniteTest = parse ["Player 1:", "43", "19",
                      "",
                      "Player 2:", "2", "29", "14"] -- should terminate

main = do
  dat <- readFile "data.txt"
  let input = parse $ lines dat

  print $ solve1 input
  -- 36257

  print $ solve2 input
  print $ solve2ST input
  -- 30795 is too low
