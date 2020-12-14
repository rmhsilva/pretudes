-- Bits! Use Integer to ensure the full range, but since I'm on a 64 bit machine
-- I could also use Int (probably).
--
-- Lesson: be careful with bits and off-by-ones.

import Data.List.Split (splitOn)
import Data.Bits (( .|. ), ( .&. ), complement)

import Data.List  (elemIndices)
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe, fromJust)
import Numeric    (readInt)

import Control.Monad (filterM)

import qualified Data.Map as Map


data Instruction =
  Mem { addr :: Integer, value :: Integer } |
  Msk { zeros :: Integer, ones :: Integer, floats :: [Int] }
  deriving Show

data VM = VM { memory :: Map.Map Integer Integer,
               bitmask :: Instruction }
  deriving Show


initvm = VM Map.empty (Msk (-1) 0 [])


-- PART 1 weird mask: the Xs are don't care.
-- The 0s change to zeros. This is an AND mask.
-- The 1s change to ones. This is an OR mask.
--
-- So it's a double mask: AND where the inverse of where the mask was 0s.
--                        OR  where the mask was 1s.

execute :: VM -> Instruction -> VM
execute vm mask@Msk {} = vm { bitmask = mask }
execute vm (Mem addr val)   = vm { memory = Map.alter newval addr (memory vm) }
  where
    newval _ = Just $ (val .&. complement (zeros (bitmask vm))) .|. ones (bitmask vm)



execute2 :: VM -> Instruction -> VM
execute2 vm mask@Msk {} = vm { bitmask = mask }
execute2 vm (Mem addr val) = vm { memory = Map.union newmem (memory vm) }
  -- Map.union favours the first arg in conflicts
  where
    newmem = Map.fromList $ zip addresses (repeat val)
    positions = floats (bitmask vm)
    powers = (2^) <$> positions
    addr' = (addr .&. complement (sum powers)) .|. ones (bitmask vm)
    addresses = (+addr') . sum <$> filterM (const [True, False]) powers


run :: (VM -> Instruction -> VM) -> [Instruction] -> VM
run execFn = foldl execFn initvm

solve fn = sum . Map.elems . memory . run fn

solve1 = solve execute
solve2 = solve execute2

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
parseMask instruction arg = Msk (readBin zeros_str) (readBin ones_str) floats
  where
    zeros_str = map (\x -> if x == '0' then '1' else '0') arg
    ones_str = map (\x -> if x == '1' then '1' else '0') arg
    floats = (length arg - 1 -) <$> elemIndices 'X' arg


parse :: String -> Instruction
parse line =
  parseInstruction instruction arg
  where
    (instruction:arg:_) = splitOn " = " line
    parseInstruction = case take 3 instruction of
      "mem" -> parseMem
      _     -> parseMask


td :: [Instruction]
td = map parse ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
                "mem[8] = 11",
                "mem[7] = 101",
                "mem[8] = 0"] -- > 165

td2 :: [Instruction]
td2 = map parse ["mask = 000000000000000000000000000000X1001X",
                 "mem[42] = 100",
                 "mask = 00000000000000000000000000000000X0XX",
                 "mem[26] = 1"] -- > 208

main = do
  dat <- readFile "data.txt"
  let input = map parse $ lines dat

  print $ solve1 input
  -- 15514035145260

  print $ solve2 input
  -- 3926790061594
