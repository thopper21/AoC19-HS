module Day2Solution
  ( solveA
  , solveB
  ) where

import           IntCode

toProgram noun verb = writeMemory 2 verb . writeMemory 1 noun . parseProgram

result = readMemory 0 . snd . run

solveA = result . toProgram 12 2

findInput nouns verbs expected input =
  head
    [ (noun, verb)
    | noun <- nouns
    , verb <- verbs
    , result (toProgram noun verb input) == expected
    ]

output (x, y) = 100 * x + y

solveB = output . findInput [0 .. 99] [0 .. 99] 19690720
