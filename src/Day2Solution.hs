module Day2Solution
  ( solveA,
    run,
    toProgram
  ) where

import           Data.IntMap.Lazy

toProgram :: [Int] -> IntMap Int
toProgram xs =
  let toProgram' [] _     = empty
      toProgram' (x:xs) i = insert i x $ toProgram' xs (i + 1)
   in toProgram' xs 0

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
    let
        run' position program =
            let opCode = program ! position
                arg offset = program ! (position + offset)
                next = run' (position + 4)
                op' = op (arg 1) (arg 2) (arg 3)
            in case opCode of
                    1  -> next $ op' program (+)
                    2  -> next $ op' program (*)
                    99 -> toInteger $ program ! 0
    in run' 0

solveA input =
  let programInts = words $ commaToSpace <$> input
   in run $ toProgram $ read <$> programInts
