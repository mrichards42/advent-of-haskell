module Advent.Y2019.IntCode (
  -- * Data and Construction
  Computer
  , fromList
  , fromVector
  , fromString

  -- * States
  , isHalted
  , isRunning
  , isWaitingForInput

  -- * Running
  , runProgram
  , runInstruction
  , runInstructions
  , addInput
  , addInputs
  , setInput
  , readOutput
  , readOutput1
  , readOutputAll
  , valAt
  ) where

import qualified Data.Char as C
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

type IntCode = Vector Int

-- | The IntCode instruction set
data Instruction = Add       Int Int Int
                 | Mult      Int Int Int
                 | Input     Int
                 | Output    Int
                 | JumpTrue  Int Int
                 | JumpFalse Int Int
                 | LessThan  Int Int Int
                 | Equals    Int Int Int
                 | Halt
                 | Invalid Int
                 deriving (Eq, Show)

-- | IntCode states
data State = Running
           | WaitInput
           | Halted
           | Error String
           deriving(Eq, Show)

-- | An IntCode computer
data Computer = Computer { cIp :: Int
                         , cMem :: IntCode
                         , cIn :: [Int]
                         , cOut :: [Int]
                         , cState :: State
                         } deriving (Eq)


-- Construction --

-- | Creates a new Computer from an IntCode Vector
fromVector :: IntCode -> Computer
fromVector mem = Computer { cIp = 0
                          , cMem = mem
                          , cIn = []
                          , cOut = []
                          , cState = Running
                          }

-- | Creates a new Computer from a List
fromList :: [Int] -> Computer
fromList = fromVector . V.fromList

-- | Reads a string of comma-separated ints, and creates a Computer
fromString :: String -> Computer
fromString inputStr = fromList . read $ "[" ++ inputStr ++ "]"


-- State --

-- | Is this Computer running?
isRunning :: Computer -> Bool
isRunning = (== Running) . cState

-- | Has this Computer halted?
isHalted :: Computer -> Bool
isHalted = (== Halted) . cState

-- | Is this Computer waiting for input?
isWaitingForInput :: Computer -> Bool
isWaitingForInput = (== WaitInput) . cState


-- Running --

data Mode = ModeImmediate
          | ModePosition
          deriving(Eq)


-- | Builds an Instruction from the current position of the computer
instructionAt :: IntCode -> Int -> Instruction
instructionAt mem ip =
  case instrCode of
    1  -> instr4 Add       val val loc
    2  -> instr4 Mult      val val loc
    3  -> instr2 Input     loc
    4  -> instr2 Output    val
    5  -> instr3 JumpTrue  val val
    6  -> instr3 JumpFalse val val
    7  -> instr4 LessThan  val val loc
    8  -> instr4 Equals    val val loc
    99 -> Halt
    x  -> Invalid x
  where
    -- instruction builders
    instr2 instr a     = instr (a 1)
    instr3 instr a b   = instr (a 1) (b 2)
    instr4 instr a b c = instr (a 1) (b 2) (c 3)
    loc n = mem V.! (ip + n)
    val n = case mode n of
              ModeImmediate -> mem V.! (ip + n)
              ModePosition -> mem V.! (mem V.! (ip + n))
    -- instruction pointer decoding
    instrRaw = mem V.! ip
    instrCode = instrRaw `mod` 100
    mode n = case digit n of
               1 -> ModeImmediate
               _ -> ModePosition
    digit n = case n of
                1 -> (instrRaw `div` 100) `mod` 10
                2 -> (instrRaw `div` 1000) `mod` 10
                3 -> (instrRaw `div` 10000) `mod` 10
                _ -> error "Instructions can only support 3 arguments"

-- | Reads a single value from the Computer's memory
valAt :: Computer -> Int -> Int
valAt Computer { cMem = mem } idx = mem V.! idx

-- | Runs a single instruction
runInstruction :: Computer -> Computer
runInstruction comp@Computer{cIp=ip, cMem=mem, cIn=input, cOut=output} =
  case instructionAt' mem ip of
    Add      a b loc -> comp { cIp = ip + 4, cMem = write loc (a + b) }
    Mult     a b loc -> comp { cIp = ip + 4, cMem = write loc (a * b) }
    Input        loc -> case input of
                          (i:is) -> comp { cIp = ip + 2
                                         , cIn = is
                                         , cMem = write loc i
                                         }
                          _      -> comp { cState = WaitInput }
    Output       val -> comp { cIp = ip + 2, cOut = output ++ [val] }
    JumpTrue   t loc -> comp { cIp = if t /= 0 then loc else ip + 3 }
    JumpFalse  t loc -> comp { cIp = if t == 0 then loc else ip + 3 }
    LessThan a b loc -> comp { cIp = ip + 4, cMem = write loc (bool (a < b)) }
    Equals   a b loc -> comp { cIp = ip + 4, cMem = write loc (bool (a == b)) }
    Halt         -> comp { cState = Halted }
    Invalid  x   -> comp { cState = Error $ "Invalid instruction: " ++ show x }
  where
    write loc v = mem V.// [(loc, v)]
    bool x = if x then 1 else 0

-- | Runs up to `n` instructions
runInstructions :: Computer -> Int -> Computer
runInstructions c n = iterate runInstruction c !! max 0 n


-- $setup
-- >>> testProgramMem = V.toList . cMem . runProgram . fromList
-- >>> testProgramInOut i = snd . readOutputAll . runProgram . flip addInput i . fromList
-- >>> prgEq8 = [3,9,8,9,10,9,4,9,99,-1,8]
-- >>> prgLt8 = [3,9,7,9,10,9,4,9,99,-1,8]
-- >>> prgEq8'= [3,3,1108,-1,8,3,4,3,99]
-- >>> prgLt8'= [3,3,1107,-1,8,3,4,3,99]
-- >>> prgCmp8 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31, 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104, 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]


-- | tests from Day02 instructions
-- >>> testProgramMem [1,0,0,0,99]
-- [2,0,0,0,99]
-- >>> testProgramMem [2,3,0,3,99]
-- [2,3,0,6,99]
-- >>> testProgramMem [2,4,4,5,99,0]
-- [2,4,4,5,99,9801]
-- >>> testProgramMem [1,1,1,4,99,5,6,0,99]
-- [30,1,1,4,2,5,6,0,99]

-- | tests from Day05 instructions
-- >>> testProgramInOut 8 prgEq8
-- [1]
-- >>> testProgramInOut 7 prgEq8
-- [0]
-- >>> testProgramInOut 4 prgLt8
-- [1]
-- >>> testProgramInOut 44 prgLt8
-- [0]
-- >>> testProgramInOut 8 prgEq8'
-- [1]
-- >>> testProgramInOut 7 prgEq8'
-- [0]
-- >>> testProgramInOut 4 prgLt8'
-- [1]
-- >>> testProgramInOut 44 prgLt8'
-- [0]
-- >>> testProgramInOut 4 prgCmp8
-- [999]
-- >>> testProgramInOut 8 prgCmp8
-- [1000]
-- >>> testProgramInOut 44 prgCmp8
-- [1001]

-- | Runs an intcode Computer to completion
runProgram :: Computer -> Computer
runProgram c = if isRunning c
                  then runProgram $ runInstruction c
                  else c

-- | Sets the first 2 params (noun, verb) of the Computer's initial instruction
setInput :: Computer -> (Int, Int) -> Computer
setInput c (noun, verb) = c { cMem = cMem c V.// [(1, noun), (2, verb)] }

-- | Adds a value to the input stream
addInput :: Computer -> Int -> Computer
addInput c@Computer{cIn=input, cState=state} v =
  c{ cIn = input ++ [v]
   , cState = if state == WaitInput then Running else state
   }

addInputs :: Computer -> [Int] -> Computer
addInputs c@Computer{cIn=input, cState=state} vs =
  c{ cIn = input ++ vs
   , cState = if state == WaitInput then Running else state
   }

-- | Reads everything from the output stream
readOutputAll :: Computer -> (Computer, [Int])
readOutputAll c@Computer{cOut=output} =
 (c{cOut = []}, output)

-- | Reads up to `n` values from the output stream
readOutput :: Computer -> Int -> (Computer, [Int])
readOutput c@Computer{cOut=output} n =
  (c{cOut = drop n output}, take n output)

-- | Reads a single value from the output stream
readOutput1 :: Computer -> (Computer, Int)
readOutput1 c@Computer{cOut=output} =
  (c{cOut = tail output}, head output)


-- Extra --

-- | A faster version of instructionAt that uses a table instead of arithmetic
instructionAt' :: IntCode -> Int -> Instruction
instructionAt' mem ip =
  case mem V.! ip of
    -- Standard set
    0001 -> Add       (val0 1) (val0 2) (loc 3)
    0002 -> Mult      (val0 1) (val0 2) (loc 3)
    0003 -> Input     (loc 1)
    0004 -> Output    (val0 1)
    0005 -> JumpTrue  (val0 1) (val0 2)
    0006 -> JumpFalse (val0 1) (val0 2)
    0007 -> LessThan  (val0 1) (val0 2) (loc 3)
    0008 -> Equals    (val0 1) (val0 2) (loc 3)
    0099 -> Halt
    -- Immediate variants
    0101 -> Add       (val1 1) (val0 2) (loc 3)
    1001 -> Add       (val0 1) (val1 2) (loc 3)
    1101 -> Add       (val1 1) (val1 2) (loc 3)
    0102 -> Mult      (val1 1) (val0 2) (loc 3)
    1002 -> Mult      (val0 1) (val1 2) (loc 3)
    1102 -> Mult      (val1 1) (val1 2) (loc 3)
    0104 -> Output    (val1 1)
    0105 -> JumpTrue  (val1 1) (val0 2)
    1005 -> JumpTrue  (val0 1) (val1 2)
    1105 -> JumpTrue  (val1 1) (val1 2)
    0106 -> JumpFalse (val1 1) (val0 2)
    1006 -> JumpFalse (val0 1) (val1 2)
    1106 -> JumpFalse (val1 1) (val1 2)
    0107 -> LessThan  (val1 1) (val0 2) (loc 3)
    1007 -> LessThan  (val0 1) (val1 2) (loc 3)
    1107 -> LessThan  (val1 1) (val1 2) (loc 3)
    0108 -> Equals    (val1 1) (val0 2) (loc 3)
    1008 -> Equals    (val0 1) (val1 2) (loc 3)
    1108 -> Equals    (val1 1) (val1 2) (loc 3)
    -- Invalid
    x    -> Invalid x
  where
    loc n = mem V.! (ip + n)
    val0 n = mem V.! (mem V.! (ip + n)) -- position mode
    val1 n = mem V.! (ip + n)           -- immediate mode


-- pretty printing
instance Show Computer where
  show Computer{cIp=ip, cMem=mem, cIn=input, cOut=output, cState=state} =
    unlines ["IntCode Computer {"
            , "State  = " ++ show state
            , "Input  = " ++ prettyPrintL input 10
            , "Output = " ++ prettyPrintL output 10
            , "IP     = " ++ show ip ++ "  [" ++ show (instructionAt' mem ip) ++ "]"
            , "Memory = " ++ prettyPrintV mem ip 10
            , "}"
            ]
    where prettyPrintL xs w = prettyPrintV (V.fromList xs) (-1) w

prettyPrintV :: Vector Int -> Int -> Int -> String
prettyPrintV vec ip w =
  if V.null vec
     then "[]"
     else "[ " ++ "length = " ++ show (V.length vec) ++ "\n" ++ unlines [showLine i | i <- [0,w..V.length vec - 1]] ++ "]"
  where
    showLine idx =
      concatMap (highlightRow idx) [ rowLabel idx
                                   , concatMap showIdx [idx..idx+w-1]
                                   , "  "
                                   , map showIdxAscii [idx..idx+w-1]
                                   ]
    rowLabel idx = pad 5 (show idx) ++ ":"
    showIdx idx =
      let v = if idx < V.length vec then show $ vec V.! idx else ""
          s = pad 8 v
      in highlightIp idx $ highlightRow idx s
    showIdxAscii idx = let n = vec V.! idx
                           c = C.chr n
                       in if idx < V.length vec && n > 0 && n < 128 && C.isAlphaNum c
                             then c
                             else '.'
    highlightIp = highlight (== ip) "\ESC[1m\ESC[33m"
    highlightRow = highlight (odd . (`div` w)) "\ESC[48;5;237m"
    highlight f escSeq idx s = if f idx then escSeq ++ s ++ "\ESC[0m" else s
    pad n s = replicate (n - length s) ' ' ++ s
