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

maxThruster program =
  maximum $ fmap (runThrusters program) (permutations [0 .. 4])

solveA = toInteger . maxThruster . parseProgram

runFeedbackThrusters input Terminated Terminated Terminated Terminated Terminated =
  input
runFeedbackThrusters input (AwaitingInput contA) (AwaitingInput contB) (AwaitingInput contC) (AwaitingInput contD) (AwaitingInput contE) =
  let (stateA, progA) = contA input
      (stateB, progB) = contB $ lastOutput progA
      (stateC, progC) = contC $ lastOutput progB
      (stateD, progD) = contD $ lastOutput progC
      (stateE, progE) = contE $ lastOutput progD
      loopOutput = lastOutput progE
   in runFeedbackThrusters loopOutput stateA stateB stateC stateD stateE

startThruster program setting =
  let (AwaitingInput resume, _) = run program
   in resume setting

startThrusters program settings =
  let [a, b, c, d, e] = fst . startThruster program <$> settings
   in runFeedbackThrusters 0 a b c d e

solveB input =
  let program = parseProgram input
   in toInteger . maximum $
      fmap (startThrusters program) (permutations [5 .. 9])
