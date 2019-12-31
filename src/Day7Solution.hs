module Day7Solution
  ( solveA
  , solveB
  ) where

import           Data.List
import           IntCode

runThrusters program = foldl runThrusterPhase 0
  where
    runThrusterPhase prevOut currIn =
      lastOutput . run $ setInput [currIn, prevOut] program

maxThruster program =
  maximum $ fmap (runThrusters program) (permutations [0 .. 4])

solveA = toInteger . maxThruster . parseProgram

solveB _ = 42
