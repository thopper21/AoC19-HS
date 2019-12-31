module Day5Solution
  ( solveA
  , solveB
  ) where

import           IntCode

solve input = toInteger . lastOutput . run . setInput input . parseProgram

solveA = solve [1]

solveB = solve [5]
