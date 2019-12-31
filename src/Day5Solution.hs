module Day5Solution
  ( solveA
  , solveB
  ) where

import           IntCode

runProgram input program =
  let
    AwaitingInput resume = run program
    Terminated program' = resume input
  in lastOutput program'

solve input = toInteger . runProgram input . parseProgram

solveA = solve 1

solveB = solve 5
