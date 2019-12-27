{-# LANGUAGE TemplateHaskell #-}

module IntCode
  ( program
  , run
  , setMemory
  , getMemory
  ) where

import           Control.Lens
import           Data.IntMap.Lazy (IntMap, empty, fromDistinctAscList, insert,
                                   lookup)
import           Data.Maybe
import           Prelude          hiding (lookup)

data Program = Program
  { _memory :: IntMap Int
  , _ip     :: Int
  , _input  :: [Int]
  , _output :: [Int]
  }

makeLenses ''Program

emptyProgram = Program {_memory = empty, _ip = 0, _input = [], _output = []}

program input = set memory (fromDistinctAscList $ zip [0 ..] input) emptyProgram

setMemory pos = over memory . insert pos

getMemory pos = fromJust . lookup pos . view memory

run program =
  let opCode = getMemory (view ip program) program
      arg offset = getMemory (view ip program + offset) program
      moveIP count = run . over ip (+ count)
      binOp fn =
        let left = getMemory (arg 1) program
            right = getMemory (arg 2) program
            result = fn left right
         in moveIP 4 $ setMemory (arg 3) result program
   in case opCode of
        1  -> binOp (+)
        2  -> binOp (*)
        99 -> program
