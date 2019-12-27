module Day2Solution
  ( solveA
  , solveB
  ) where

import           Data.List.Split (splitOn)
import           IntCode

parseInput = fmap read . splitOn ","

toProgram noun verb = setMemory 2 verb . setMemory 1 noun . program

result = toInteger . getMemory 0 . run

solveA = result . toProgram 12 2 . parseInput

findInput nouns verbs expected input =
  head
    [ (noun, verb)
    | noun <- nouns
    , verb <- verbs
    , result (toProgram noun verb input) == expected
    ]

output (x, y) = 100 * x + y

solveB =
  toInteger . output . findInput [0 .. 99] [0 .. 99] 19690720 . parseInput
