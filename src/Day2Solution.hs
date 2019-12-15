module Day2Solution
  ( solveA
  , solveB
  ) where

import           Data.IntMap.Lazy
import           Data.List        (find)
import           Data.Maybe

toProgram :: Int -> Int -> [Int] -> IntMap Int
toProgram noun verb xs =
  let toProgram' [] _     = empty
      toProgram' (x:xs) i = insert i x $ toProgram' xs (i + 1)
   in insert 2 verb $ insert 1 noun $ toProgram' xs 0

commaToSpace ',' = ' '
commaToSpace c   = c

replaceAt [] _ _           = []
replaceAt (x:xs) 0 value   = value : xs
replaceAt (x:xs) pos value = x : replaceAt xs (pos - 1) value

op leftPos rightPos outPos program fn =
  let left = program ! leftPos
      right = program ! rightPos
      result = fn left right
   in insert outPos result program

run =
  let run' position program =
        let opCode = program ! position
            arg offset = program ! (position + offset)
            next = run' (position + 4)
            op' = op (arg 1) (arg 2) (arg 3)
         in case opCode of
              1  -> next $ op' program (+)
              2  -> next $ op' program (*)
              99 -> toInteger $ program ! 0
   in run' 0

parseInput input = read <$> words (commaToSpace <$> input)

solveA input = run $ toProgram 12 2 $ parseInput input

solveB input =
  let parsed = parseInput input
      pred (noun, verb) = 19690720 == run (toProgram noun verb parsed)
      (noun, verb) =
        fromJust $
        find pred [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]
   in toInteger $ 100 * noun + verb
