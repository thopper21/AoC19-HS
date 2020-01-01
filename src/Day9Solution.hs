module Day9Solution
  ( solveA
  ) where

import           IntCode

runProgram input program =
  let (AwaitingInput resume, _) = run program
      (Terminated, program') = resume input
   in lastOutput program'

solveA = runProgram 1 . parseProgram
