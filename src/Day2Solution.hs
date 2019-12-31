module Day2Solution
  ( solveA
  , solveB
  ) where

import           IntCode

toProgram noun verb = writeMem 2 verb . writeMem 1 noun . parseProgram

result program =
  let (Terminated, program') = run program
   in readMem 0 program'

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
