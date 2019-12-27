module Day2Solution
  ( solveA
  , solveB
  ) where

import           Data.List       (find)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)
import           IntCode

parseInput = fmap read . splitOn ","

toProgram noun verb = set 2 verb . set 1 noun . program

result = toInteger . get 0 . run

solveA input = result $ toProgram 12 2 $ parseInput input

solveB input =
  let parsed = parseInput input
      pred (noun, verb) = result (toProgram noun verb parsed) == 19690720
      (noun, verb) =
        fromJust $
        find pred [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]
   in toInteger $ 100 * noun + verb
