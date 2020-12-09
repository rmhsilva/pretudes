-- That was fun.
module Dec8 where

import Data.Sequence (Seq, fromList, findIndicesL, index, adjust)

data Instruction = Instruction {op :: String, arg :: Int}
  deriving Show

data VM = VM {addr :: Int, acc :: Int, code :: Seq Instruction}
  deriving Show


-- Execute an instruction on a VM
execute :: VM -> Instruction -> VM
execute vm Instruction{op="nop", arg=n} = vm { addr = addr vm + 1 }
execute vm Instruction{op="acc", arg=n} = vm { addr = addr vm + 1, acc = acc vm + n }
execute vm Instruction{op="jmp", arg=n} = vm { addr = addr vm + n }
execute vm i = error ("Bad instruction: " ++ show i)


-- run a VM until termination (or a loop begins)
runVM :: [Int] -> VM -> VM
runVM history vm
  | addr vm `elem` history = vm
  | addr vm == length (code vm) = vm  -- for part 2
  | otherwise = runVM (addr vm:history) new_vm
  where
    new_vm = execute vm (index (code vm) (addr vm))


-- initialise and run a VM to completion
initAndRun :: Seq Instruction -> VM
initAndRun instrs = runVM [] $ VM 0 0 instrs


-- switch an instruction type for part2
switch :: Instruction -> Instruction
switch Instruction{op="jmp", arg=arg} = Instruction "nop" arg
switch Instruction{op="nop", arg=arg} = Instruction "jmp" arg
switch i = i


-- find the first VM where the addr at termination is equal to the program length
solve2 :: Seq Instruction -> VM
solve2 instrs =
  head $ filter ((==length instrs) . addr) $ map initAndRun candidatePrograms
  where
    indices = findIndicesL (`elem` ["nop", "jmp"]) $ fmap op instrs
    candidatePrograms = map (\i -> adjust switch i instrs) indices

--

parseLine l = Instruction (take 3 l) (read (filter (/='+') (drop 3 l)) ::Int)

td :: Seq Instruction
td = fromList $ map parseLine ["nop +0",
                               "acc +1",
                               "jmp +4",
                               "acc +3",
                               "jmp -3",
                               "acc -99",
                               "acc +1",
                               "jmp -4",
                               "acc +6"]

main = do
  dat <- readFile "data.txt"
  let ls = fromList $ map parseLine $ lines dat

  print $ acc $ initAndRun ls
  -- 1384

  print $ acc $ solve2 ls
  -- 761
