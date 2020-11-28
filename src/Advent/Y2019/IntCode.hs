{- 1202 Program Alarm -}
module Advent.Y2019.IntCode (
  Computer
  , isHalted
  , fromList
  , fromVector
  , fromString
  , valAt
  , runProgram
  , setInput
  ) where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

type IntCode = Vector Int

-- | The IntCode instruction set
data Instruction = Add Int Int Int
                 | Mult Int Int Int
                 | Halt
                 deriving (Eq)

-- | An IntCode computer
data Computer = Computer { cIp :: Int
                         , cMem :: IntCode
                         } deriving (Show, Eq)

-- | Creates a new Computer from an IntCode Vector
fromVector :: IntCode -> Computer
fromVector mem = Computer { cIp = 0, cMem = mem }

-- | Creates a new Computer from a List
fromList :: [Int] -> Computer
fromList = fromVector . V.fromList

-- | Reads a string of comma-separated ints, and creates a Computer
fromString :: String -> Computer
fromString inputStr = fromList . read $ "[" ++ inputStr ++ "]"

-- | Has this Computer halted?
isHalted :: Computer -> Bool
isHalted Computer{cIp = ip, cMem = mem} =
  instructionAt mem ip == Halt

-- | Builds an Instruction from the current position of the computer
instructionAt :: IntCode -> Int -> Instruction
instructionAt mem ip =
  case mem V.! ip of
    1  -> instr4 Add  pos pos out
    2  -> instr4 Mult pos pos out
    99 -> Halt
    x  -> error $ "Invalid instruction: " ++ show x
  where
    instr4 instr a b c = instr (a $ ip + 1) (b $ ip + 2) (c $ ip + 3)
    pos x = mem V.! (mem V.! x)
    out x = mem V.! x

-- | Reads a single value from the Computer's memory
valAt :: Computer -> Int -> Int
valAt Computer { cMem = mem } idx = mem V.! idx

-- | Runs a single instruction
runInstruction :: Computer -> Computer
runInstruction comp@Computer{cIp=ip, cMem=mem} =
  case instructionAt mem ip of
    Add  a b out -> comp {cIp = ip + 4, cMem = mem V.// [(out, a + b)]}
    Mult a b out -> comp {cIp = ip + 4, cMem = mem V.// [(out, a * b)]}
    Halt         -> comp

-- tests from Day02 instructions
-- $setup
-- >>> testProgram = V.toList . cMem . runProgram . fromList
-- >>> a = 1234

-- | Runs an intcode Computer to completion
-- >>> testProgram [1,0,0,0,99]
-- [2,0,0,0,99]
-- >>> testProgram [2,3,0,3,99]
-- [2,3,0,6,99]
-- >>> testProgram [2,4,4,5,99,0]
-- [2,4,4,5,99,9801]
-- >>> testProgram [1,1,1,4,99,5,6,0,99]
-- [30,1,1,4,2,5,6,0,99]
runProgram :: Computer -> Computer
runProgram c = if isHalted c
                  then c
                  else runProgram $ runInstruction c

-- | Sets the first 2 params (noun, verb) of the Computer's initial instruction
setInput :: Computer -> (Int, Int) -> Computer
setInput c (noun, verb) = c { cMem = cMem c V.// [(1, noun), (2, verb)] }
