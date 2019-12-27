{-# LANGUAGE TemplateHaskell #-}

module IntCode
  ( program
  , run
  , setMemory
  , getMemory
  ) where

import           Control.Lens
import           Data.IntMap.Lazy (IntMap, fromDistinctAscList, insert, (!))

data Program = Program
  { _memory :: IntMap Int
  , _ip     :: Int
  , _input  :: [Int]
  , _output :: [Int]
  }

makeLenses ''Program

program = fromDistinctAscList . zip [0 ..]

setMemory = insert

getMemory = flip (!)

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
              99 -> program
   in run' 0
