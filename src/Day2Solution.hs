module Day2Solution
  ( solveA
  , solveB
  ) where

import           Data.List
import           Data.Maybe
import           IntCode

toProgram noun verb = writeMemory 2 verb . writeMemory 1 noun . parseProgram

result = readMemory 0 . snd . run

solveA = result . toProgram 12 2

findInput nouns verbs expected input =
  fromJust . find outputsExpected $ (,) <$> nouns <*> verbs
  where
    outputsExpected (noun, verb) =
      (== expected) . result $ toProgram noun verb input

output (x, y) = 100 * x + y

solveB = output . findInput [0 .. 99] [0 .. 99] 19690720
