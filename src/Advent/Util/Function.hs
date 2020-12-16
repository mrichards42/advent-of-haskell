{- Misc Functions -}
module Advent.Util.Function (
    fixedPoint
  ) where

-- | Finds a fixed point of function `f` for input `x`, i.e. iterates until the
-- input equals the output.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x =
  let x' = f x
  in if x == x'
       then x'
       else fixedPoint f x'
