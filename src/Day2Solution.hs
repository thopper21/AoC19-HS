module Day2Solution
  ( solveA
  , solveB
  ) where

import           Data.IntMap.Lazy (IntMap, empty, insert, (!))
import           Data.List        (find, foldl)
import           Data.Maybe       (fromJust)

toProgram :: Int -> Int -> [Int] -> IntMap Int
toProgram noun verb =
  let fromList xs =
        foldl (\program (i, x) -> insert i x program) empty (zip [0 ..] xs)
   in insert 2 verb . insert 1 noun . fromList

replace value with =
  let replacement c =
        if c == value
          then with
          else c
   in replacement

run =
  let run' position program =
        let opCode = program ! position
            arg offset = program ! (position + offset)
            next = run' (position + 4)
            binOp fn program =
              let left = program ! arg 1
                  right = program ! arg 2
                  result = fn left right
               in insert (arg 3) result program
         in case opCode of
              1  -> next $ binOp (+) program
              2  -> next $ binOp (*) program
              99 -> toInteger $ program ! 0
   in run' 0

parseInput input = read <$> words (replace ',' ' ' <$> input)

solveA input = run $ toProgram 12 2 $ parseInput input

solveB input =
  let parsed = parseInput input
      pred (noun, verb) = 19690720 == run (toProgram noun verb parsed)
      (noun, verb) =
        fromJust $
        find pred [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]
   in toInteger $ 100 * noun + verb
