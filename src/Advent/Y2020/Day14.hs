{- Docking Data -}
module Advent.Y2020.Day14 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import Data.Bits ((.&.), (.|.))
import Data.List (foldl')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Numeric (readInt)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

{- Part 1

As your ferry approaches the sea port, the captain asks for your help again.
The computer system that runs this port isn't compatible with the docking
program on the ferry, so the docking parameters aren't being correctly
initialized in the docking program's memory.

After a brief inspection, you discover that the sea port's computer system uses
a strange bitmask system in its initialization program. Although you don't have
the correct decoder chip handy, you can emulate it in software!

The initialization program (your puzzle input) can either update the bitmask or
write a value to memory. Values and memory addresses are both 36-bit unsigned
integers. For example, ignoring bitmasks for a moment, a line like mem[8] = 11
would write the value 11 to memory address 8.

The bitmask is always given as a string of 36 bits, written with the most
significant bit (representing 2^35) on the left and the least significant bit
(2^0, that is, the 1s bit) on the right. The current bitmask is applied to
values immediately before they are written to memory: a 0 or 1 overwrites the
corresponding bit in the value, while an X leaves the bit in the value
unchanged.

For example, consider the following program:

    mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
    mem[8] = 11
    mem[7] = 101
    mem[8] = 0

This program starts by specifying a bitmask (mask = ....). The mask it
specifies will overwrite two bits in every written value: the 2s bit is
overwritten with 0, and the 64s bit is overwritten with 1.

The program then attempts to write the value 11 to memory address 8. By
expanding everything out to individual bits, the mask is applied as follows:

    value:  000000000000000000000000000000001011  (decimal 11)
    mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
    result: 000000000000000000000000000001001001  (decimal 73)

So, because of the mask, the value 73 is written to memory address 8 instead.
Then, the program tries to write 101 to address 7:

    value:  000000000000000000000000000001100101  (decimal 101)
    mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
    result: 000000000000000000000000000001100101  (decimal 101)

This time, the mask has no effect, as the bits it overwrote were already the
values the mask tried to set. Finally, the program tries to write 0 to address
8:

    value:  000000000000000000000000000000000000  (decimal 0)
    mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
    result: 000000000000000000000000000001000000  (decimal 64)

64 is written to address 8 instead, overwriting the value that was there
previously.

To initialize your ferry's docking program, you need the sum of all values left
in memory after the initialization program completes. (The entire 36-bit
address space begins initialized to the value 0 at every address.) In the above
example, only two values in memory are not zero - 101 (at address 7) and 64 (at
address 8) - producing a sum of 165.

Execute the initialization program. What is the sum of all values left in
memory after it completes?

-}

-- Mask type --

newtype Mask = Mask (Int, Int, [Mask])
  deriving (Show)

simpleMask :: Int -> Int -> Mask
simpleMask ones zeroes = Mask (ones, zeroes, [])

emptyMask :: Mask
emptyMask = simpleMask 0 0

readMask :: String -> Mask
readMask bits = let Mask (ones, zeroes, _) = readSimpleMask bits
                    xs = readXMasks bits
                in Mask (ones, zeroes, xs)

binToInt :: String -> Int
binToInt = fst . head . readInt 2 validDigit read1
  where validDigit '0' = True
        validDigit '1' = True
        validDigit _ = False
        read1 x = read [x]

readXMasks :: String -> [Mask]
readXMasks =
  map readSimpleMask . allPossibilities
  where allPossibilities :: String -> [String]
        allPossibilities (x:rest) = do
          x' <- if x == 'X' then ['0', '1'] else ['X']
          y <- allPossibilities rest
          return $ x' : y
        allPossibilities [] = [""]

readSimpleMask :: String -> Mask
readSimpleMask bits = simpleMask (readOnes bits) (readZeroes bits)
  where readOnes = binToInt . replace 'X' '0'
        readZeroes = binToInt . replace 'X' '1'
        replace a b = map (\x -> if x == a then b else x)


-- Instruction / computer types --

data Instruction = WriteMem Int Int
                 | SetMask Mask
                 deriving (Show)

type Memory = IntMap Int
type Computer = (Memory, Mask)


-- Parser --

pMask :: Parser Mask
pMask = readMask <$> M.some (M.oneOf "X01")

pInstruction :: Parser Instruction
pInstruction = M.choice [ setMask, writeMem ]
  where setMask = do
          _ <- M.string "mask = "
          mask <- pMask
          return $ SetMask mask
        writeMem = do
          _ <- M.string "mem["
          addr <- L.decimal
          _ <- M.string "] = "
          val <- L.decimal
          return $ WriteMem addr val

parseInput :: String -> [Instruction]
parseInput = parseOrError (pInstruction `M.sepEndBy1` M.newline)


-- Part 1 --

-- >>> applyMask1 (simpleMask 64 125) 11
-- 73
-- >>> applyMask1 (simpleMask 64 125) 101
-- 101
applyMask1 :: Mask -> Int -> Int
applyMask1 (Mask (ones, zeroes, _)) val = (val .|. ones) .&. zeroes

applyInstruction1 :: Computer -> Instruction -> Computer
applyInstruction1 (mem, _) (SetMask mask) = (mem, mask)
applyInstruction1 (mem, mask) (WriteMem addr val) =
  let mem' = IM.insert addr (applyMask1 mask val) mem
  in (mem', mask)

-- | >>> part1 "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"
-- 165
-- >>> part1 <$> readFile "input/2020/day14.txt"
-- 6386593869035
part1 :: String -> Int
part1 input = let instructions = parseInput input
                  comp = (IM.empty, emptyMask)
                  (mem, _) = foldl' applyInstruction1 comp instructions
              in sum (IM.elems mem)



{- Part 2

For some reason, the sea port's computer system still can't communicate with
your ferry's docking program. It must be using version 2 of the decoder chip!

A version 2 decoder chip doesn't modify the values being written at all.
Instead, it acts as a memory address decoder. Immediately before a value is
written to memory, each bit in the bitmask modifies the corresponding bit of
the destination memory address in the following way:

  - If the bitmask bit is 0, the corresponding memory address bit is unchanged.
  - If the bitmask bit is 1, the corresponding memory address bit is
    overwritten with 1.
  - If the bitmask bit is X, the corresponding memory address bit is floating.

A floating bit is not connected to anything and instead fluctuates
unpredictably. In practice, this means the floating bits will take on all
possible values, potentially causing many memory addresses to be written all at
once!

For example, consider the following program:

    mask = 000000000000000000000000000000X1001X
    mem[42] = 100
    mask = 00000000000000000000000000000000X0XX
    mem[26] = 1

When this program goes to write to memory address 42, it first applies the
bitmask:

    address: 000000000000000000000000000000101010  (decimal 42)
    mask:    000000000000000000000000000000X1001X
    result:  000000000000000000000000000000X1101X

After applying the mask, four bits are overwritten, three of which are
different, and two of which are floating. Floating bits take on every possible
combination of values; with two floating bits, four actual memory addresses are
written:

    000000000000000000000000000000011010  (decimal 26)
    000000000000000000000000000000011011  (decimal 27)
    000000000000000000000000000000111010  (decimal 58)
    000000000000000000000000000000111011  (decimal 59)

Next, the program is about to write to memory address 26 with a different
bitmask:

    address: 000000000000000000000000000000011010  (decimal 26)
    mask:    00000000000000000000000000000000X0XX
    result:  00000000000000000000000000000001X0XX

This results in an address with three floating bits, causing writes to eight
memory addresses:

    000000000000000000000000000000010000  (decimal 16)
    000000000000000000000000000000010001  (decimal 17)
    000000000000000000000000000000010010  (decimal 18)
    000000000000000000000000000000010011  (decimal 19)
    000000000000000000000000000000011000  (decimal 24)
    000000000000000000000000000000011001  (decimal 25)
    000000000000000000000000000000011010  (decimal 26)
    000000000000000000000000000000011011  (decimal 27)

The entire 36-bit address space still begins initialized to the value 0 at
every address, and you still need the sum of all values left in memory at the
end of the program. In this example, the sum is 208.

Execute the initialization program using an emulator for a version 2 decoder
chip. What is the sum of all values left in memory after it completes?

-}

-- >>> applyMask2 (readMask "000000000000000000000000000000X1001X") 42
-- [26,27,58,59]
-- >>> applyMask2 (readMask "00000000000000000000000000000000X0XX") 26
-- [16,17,18,19,24,25,26,27]
applyMask2 :: Mask -> Int -> [Int]
applyMask2 (Mask (ones, _, xs)) addr =
  [ applyMask1 x (addr .|. ones) | x <- xs ]

applyInstruction2 :: Computer -> Instruction -> Computer
applyInstruction2 (mem, _) (SetMask mask) = (mem, mask)
applyInstruction2 (mem, mask) (WriteMem addr val) =
  let addrs = applyMask2 mask addr
      mem' = IM.union (IM.fromList $ zip addrs (repeat val)) mem
  in (mem', mask)

-- | >>> part2 "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1"
-- 208
-- | >>> part2 <$> readFile "input/2020/day14.txt"
-- 4288986482164
part2 :: String -> Int
part2 input = let instructions = parseInput input
                  comp = (IM.empty, emptyMask)
                  (mem, _) = foldl' applyInstruction2 comp instructions
              in sum (IM.elems mem)
