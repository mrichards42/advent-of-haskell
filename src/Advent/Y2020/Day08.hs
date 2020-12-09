{- Handheld Halting -}
module Advent.Y2020.Day08 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import qualified Advent.Util.Parsing as P
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

{- Part 1

Your flight to the major airline hub reaches cruising altitude without
incident. While you consider checking the in-flight menu for one of those
drinks that come with a little umbrella, you are interrupted by the kid sitting
next to you.

Their handheld game console won't turn on! They ask if you can take a look.

You narrow the problem down to a strange infinite loop in the boot code (your
puzzle input) of the device. You should be able to fix it, but first you need
to be able to run the code in isolation.

The boot code is represented as a text file with one instruction per line of
text. Each instruction consists of an operation (acc, jmp, or nop) and an
argument (a signed number like +4 or -20).

  - acc increases or decreases a single global value called the accumulator by
    the value given in the argument. For example, acc +7 would increase the
    accumulator by 7. The accumulator starts at 0. After an acc instruction,
    the instruction immediately below it is executed next.
  - jmp jumps to a new instruction relative to itself. The next instruction to
    execute is found using the argument as an offset from the jmp instruction;
    for example, jmp +2 would skip the next instruction, jmp +1 would continue
    to the instruction immediately below it, and jmp -20 would cause the
    instruction 20 lines above to be executed next.
  - nop stands for No OPeration - it does nothing. The instruction immediately
    below it is executed next.

For example, consider the following program:

  nop +0
  acc +1
  jmp +4
  acc +3
  jmp -3
  acc -99
  acc +1
  jmp -4
  acc +6

These instructions are visited in this order:

  nop +0  | 1
  acc +1  | 2, 8(!)
  jmp +4  | 3
  acc +3  | 6
  jmp -3  | 7
  acc -99 |
  acc +1  | 4
  jmp -4  | 5
  acc +6  |

First, the nop +0 does nothing. Then, the accumulator is increased from 0 to 1
(acc +1) and jmp +4 sets the next instruction to the other acc +1 near the
bottom. After it increases the accumulator from 1 to 2, jmp -4 executes,
setting the next instruction to the only acc +3. It sets the accumulator to 5,
and jmp -3 causes the program to continue back at the first acc +1.

This is an infinite loop: with this sequence of jumps, the program will run
forever. The moment the program tries to run any instruction a second time, you
know it will never terminate.

Immediately before the program would run an instruction a second time, the
value in the accumulator is 5.

Run your copy of the boot code. Immediately before any instruction is executed
a second time, what value is in the accumulator?

-}

-- Computer

data Instruction = Nop Int
                 | Acc Int
                 | Jmp Int
                 deriving (Show, Eq)

data State = Running
           | Halted
           | Loop
           deriving (Show, Eq)

type Code = Vector Instruction

data Computer = Computer { cCode :: Code
                         , cAcc :: Int
                         , cIp :: Int
                         , cState :: State
                         } deriving (Show)

computerFromCode :: Code -> Computer
computerFromCode code = Computer code 0 0 Running

runInstruction :: Computer -> Computer
runInstruction c@Computer{cCode=code, cAcc=acc, cIp=ip} =
  case code V.!? ip of
    Just (Nop _) -> c{cIp = ip + 1}
    Just (Acc x) -> c{cIp = ip + 1, cAcc = acc + x}
    Just (Jmp x) -> c{cIp = ip + x}
    Nothing      -> c{cState = Halted}

runUntilLoop :: Computer -> Computer
runUntilLoop = runUntilLoop' Set.empty
  where
    runUntilLoop' :: Set Int -> Computer -> Computer
    runUntilLoop' seen c@Computer{cIp=ip}
      | ip `Set.member` seen = c
      | otherwise            = runUntilLoop' (Set.insert ip seen) (runInstruction c)

-- Parsing

pInstruction :: Parser Instruction
pInstruction =
  M.choice [ Nop <$ M.string "nop " <*> pNumber
           , Acc <$ M.string "acc " <*> pNumber
           , Jmp <$ M.string "jmp " <*> pNumber
           ]
  where pNumber = L.signed M.space L.decimal :: Parser Int

pCode :: Parser Code
pCode = V.fromList <$> pInstruction `P.sepByNG` M.newline

parseInput :: String -> Computer
parseInput = computerFromCode . parseOrError pCode

-- | >>> part1 "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6\n"
-- 5
-- >>> part1 <$> readFile "input/2020/day08.txt"
-- 1723
part1 :: String -> Int
part1 = cAcc . runUntilLoop . parseInput


{- Part 2

After some careful analysis, you believe that exactly one instruction is
corrupted.

Somewhere in the program, either a jmp is supposed to be a nop, or a nop is
supposed to be a jmp. (No acc instructions were harmed in the corruption of
this boot code.)

The program is supposed to terminate by attempting to execute an instruction
immediately after the last instruction in the file. By changing exactly one jmp
or nop, you can repair the boot code and make it terminate correctly.

For example, consider the same program from above:

  nop +0
  acc +1
  jmp +4
  acc +3
  jmp -3
  acc -99
  acc +1
  jmp -4
  acc +6

If you change the first instruction from nop +0 to jmp +0, it would create a
single-instruction infinite loop, never leaving that instruction. If you change
almost any of the jmp instructions, the program will still eventually find
another jmp instruction and loop forever.

However, if you change the second-to-last instruction (from jmp -4 to nop -4),
the program terminates! The instructions are visited in this order:

  nop +0  | 1
  acc +1  | 2
  jmp +4  | 3
  acc +3  |
  jmp -3  |
  acc -99 |
  acc +1  | 4
  nop -4  | 5
  acc +6  | 6

After the last instruction (acc +6), the program terminates by attempting to
run the instruction below the last instruction in the file. With this change,
after the program terminates, the accumulator contains the value 8 (acc +1, acc
+1, acc +6).

Fix the program so that it terminates normally by changing exactly one jmp (to
nop) or nop (to jmp). What is the value of the accumulator after the program
terminates?

-}

-- | Runs a program until it either halts or ends up in an infinite loop
runUntilDone :: Computer -> Computer
runUntilDone = runUntilDone' Set.empty
  where
    runUntilDone' :: Set Int -> Computer -> Computer
    runUntilDone' seen c@Computer{cIp=ip, cState=state}
      | state == Halted      = c
      | ip `Set.member` seen = c{cState = Loop}
      | otherwise            = runUntilDone' (Set.insert ip seen) (runInstruction c)

solve2 :: Computer -> Computer
solve2 comp = head . dropWhile ((== Loop) . cState) . map runUntilDone $ candidates
  where
    candidates :: [Computer]
    candidates = mapMaybe (editInstruction comp) [1..V.length (cCode comp)]

    editInstruction :: Computer -> Int -> Maybe Computer
    editInstruction c@Computer{cCode=code} n =
      case code V.!? n of
        Just (Nop x) -> Just c{cCode = code V.// [(n, Jmp x)]}
        Just (Jmp x) -> Just c{cCode = code V.// [(n, Nop x)]}
        _            -> Nothing

-- | >>> part2 "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6\n"
-- 8
-- >>> part2 <$> readFile "input/2020/day08.txt"
-- 846
part2 :: String -> Int
part2 = cAcc . solve2 . parseInput
