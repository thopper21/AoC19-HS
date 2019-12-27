module Day2Solution
  ( solveA
  , solveB
  ) where

import           Data.List        (find, foldl)
import           Data.List.Split  (splitOn)
import           Data.Maybe       (fromJust)
import           IntCode

parseInput = fmap read . splitOn ","

solveA input = run $ toProgram 12 2 $ parseInput input

solveB input =
  let parsed = parseInput input
      pred (noun, verb) = run (toProgram noun verb parsed) == 19690720
      (noun, verb) =
        fromJust $
        find pred [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]
   in toInteger $ 100 * noun + verb
