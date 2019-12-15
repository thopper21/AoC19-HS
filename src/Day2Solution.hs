module Day2Solution
  ( solveA
  , solveB
  ) where

import           Data.IntMap.Lazy (IntMap, fromDistinctAscList, insert, (!))
import           Data.List        (find, foldl)
import           Data.List.Split  (splitOn)
import           Data.Maybe       (fromJust)

toProgram noun verb =
  let fromList xs = fromDistinctAscList $ zip [0 ..] xs
      setInput noun verb = insert 2 verb . insert 1 noun
   in setInput noun verb . fromList

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

parseInput = fmap read . splitOn ","

solveA input = run $ toProgram 12 2 $ parseInput input

solveB input =
  let parsed = parseInput input
      pred (noun, verb) = run (toProgram noun verb parsed) == 19690720
      (noun, verb) =
        fromJust $
        find pred [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]
   in toInteger $ 100 * noun + verb
