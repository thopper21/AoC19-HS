module Day9Solution
  ( solveA
  , solveB
  ) where

import           IntCode

runProgram input = lastOutput . snd . runWithInput [input]

solveA = runProgram 1 . parseProgram

solveB = runProgram 2 . parseProgram
