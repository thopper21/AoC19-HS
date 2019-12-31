module Day7Solution
  ( solveA
  , solveB
  ) where

import           Data.List
import           IntCode

runFeedbackThrusters input Terminated _ _ _ _ = input
runFeedbackThrusters input (AwaitingInput resume) stateB stateC stateD stateE =
  let (stateA, program) = resume input
      input' = lastOutput program
   in runFeedbackThrusters input' stateB stateC stateD stateE stateA

startThruster program setting =
  let (AwaitingInput resume, _) = run program
   in resume setting

runThrusters program settings =
  let [a, b, c, d, e] = fst . startThruster program <$> settings
   in runFeedbackThrusters 0 a b c d e

maxSignal settings program =
  maximum $ runThrusters program <$> permutations settings

solve settings = toInteger . maxSignal settings . parseProgram

solveA = solve [0 .. 4]

solveB = solve [5 .. 9]
