module Day7Solution
  ( solveA
  , solveB
  ) where

import           Data.List
import           IntCode

runThrusters program = foldl runThrusterPhase 0
  where
    runThrusterPhase prevOut currIn =
      let (AwaitingInput resume, _) = run program
          (AwaitingInput resume', _) = resume currIn
          (Terminated, program') = resume' prevOut
       in lastOutput program'

maxThruster program = maximum $ runThrusters program <$> permutations [0 .. 4]

solveA = toInteger . maxThruster . parseProgram

runFeedbackThrusters input Terminated _ _ _ _ = input
runFeedbackThrusters input (AwaitingInput resume) stateB stateC stateD stateE =
  let (stateA, program) = resume input
      loopOutput = lastOutput program
   in runFeedbackThrusters loopOutput stateB stateC stateD stateE stateA

startThruster program setting =
  let (AwaitingInput resume, _) = run program
   in resume setting

startThrusters program settings =
  let [a, b, c, d, e] = fst . startThruster program <$> settings
   in runFeedbackThrusters 0 a b c d e

solveB input =
  let program = parseProgram input
   in toInteger . maximum $ startThrusters program <$> permutations [5 .. 9]
