module IntCode
  ( toProgram
  , run
  ) where

import           Data.IntMap.Lazy (IntMap, fromDistinctAscList, insert, (!))

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
