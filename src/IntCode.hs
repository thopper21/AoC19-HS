{-# LANGUAGE TemplateHaskell #-}

module IntCode
  ( program
  , run
  , setMemory
  , getMemory
  ) where

import           Control.Lens
import           Data.IntMap.Lazy (IntMap, empty, fromDistinctAscList, insert, (!))

data Program = Program
  { _memory :: IntMap Int
  , _ip     :: Int
  , _input  :: [Int]
  , _output :: [Int]
  }

makeLenses ''Program

emptyProgram = Program { _memory = empty, _ip = 0, _input = [], _output = []}

program input = set memory (fromDistinctAscList $ zip [0 ..] input) emptyProgram

setMemory pos val = over memory $ insert pos val

getMemory pos program = view memory program ! pos

run program =
    let
        opCode = getMemory (view ip program) program
        arg offset = getMemory (view ip program + offset) program
        binOp fn program =
            let left = getMemory (arg 1) program
                right = getMemory (arg 2) program
                result = fn left right
            in setMemory (arg 3) result program
        next program = run $ over ip (+ 4) program
        in case opCode of
            1 -> next $ binOp (+) program
            2 -> next $ binOp (*) program
            99 -> program
