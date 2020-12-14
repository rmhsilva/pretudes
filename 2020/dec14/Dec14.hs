-- Bits! Use Integer to ensure the full range, but since I'm on a 64 bit machine
-- I could also use Int (probably).

import Data.List.Split (splitOn)
import Data.Bits (( .|. ), ( .&. ), complement)

import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe, fromJust)
import Numeric    (readInt)

import qualified Data.Map as Map


data Instruction =
  Mem { addr :: Integer, value :: Integer } |
  Msk { and_mask :: Integer, or_mask :: Integer }
  deriving Show

data VM = VM { memory :: Map.Map Integer Integer,
               bitmask :: Instruction }
  deriving Show


initvm = VM Map.empty (Msk (-1) 0)

execute :: VM -> Instruction -> VM
execute vm mask@(Msk _ _) = vm { bitmask = mask }
execute vm (Mem addr val) = vm { memory = Map.alter newval addr (memory vm) }
  where
    newval _ = Just $ (val .&. and_mask (bitmask vm)) .|. or_mask (bitmask vm)


run :: [Instruction] -> VM
run = foldl execute initvm

solve1 = sum . Map.elems . memory . run

---


-- https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
readBin :: Integral a => String -> a
readBin = fst . fromJust . listToMaybe . readInt 2 (`elem` "01") digitToInt


parseMem :: String -> String -> Instruction
parseMem instruction arg = Mem addr val
  where
    addr = read (takeWhile (/=']') (drop 4 instruction)) :: Integer
    val = read arg :: Integer


parseMask :: String -> String -> Instruction
parseMask instruction arg = Msk (complement (readBin and_str)) (readBin or_str)
  where
    and_str = map (\x -> if x == '0' then '1' else '0') arg
    or_str = map (\x -> if x == '1' then '1' else '0') arg


-- converting binary to decimal: replace all the chars I don't care about, then
-- readBinary.

-- weird mask: the Xs are don't care.
-- The 0s change to zeros. This is an AND mask.
-- The 1s change to ones. This is an OR mask.
--
-- So it's a double mask: AND where the inverse of where the mask was 0s.
--                        OR  where the mask was 1s.

parse :: String -> Instruction
parse line =
  parseInstruction instruction arg
  where
    (instruction:arg:_) = splitOn " = " line
    parseInstruction = case take 3 instruction of
      "mem" -> parseMem
      _     -> parseMask



td = map parse ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
                "mem[8] = 11",
                "mem[7] = 101",
                "mem[8] = 0"]

main = do
  dat <- readFile "data.txt"
  let input = map parse $ lines dat

  print $ solve1 input
