module Day5Solution
  ( solveA
  , solveB
  ) where

import           Data.List.Split (splitOn)
import           IntCode

solve input =
  toInteger .
  diagnosticCode . run . setInput input . program . fmap read . splitOn ","

solveA = solve [1]

solveB = solve [5]
