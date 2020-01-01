module Day5Solution
  ( solveA
  , solveB
  ) where

import           IntCode

runProgram input = lastOutput . snd . runWithInput [input]

solve input = runProgram input . parseProgram

solveA = solve 1

solveB = solve 5
