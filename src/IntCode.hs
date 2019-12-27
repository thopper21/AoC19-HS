{-# LANGUAGE TemplateHaskell #-}

module IntCode
  ( program
  , run
  , writeMem
  , readMem
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

data TernaryOp
  = Add
  | Mult

data UnaryOp
  = In
  | Out

data NullaryOp =
  Terminate

data ParamMode
  = Immediate
  | Position

data Operation
  = Nullary NullaryOp
  | Unary UnaryOp
          ParamMode
  | Ternary TernaryOp
            ParamMode
            ParamMode
            ParamMode

makeLenses ''Program

emptyProgram = Program {_memory = empty, _ip = 0, _input = [], _output = []}

program input = set memory (fromDistinctAscList $ zip [0 ..] input) emptyProgram

writeMem pos = over memory . insert pos

readMem pos = fromJust . lookup pos . view memory

operation 1  = Ternary Add Position Position Position
operation 2  = Ternary Mult Position Position Position
operation 99 = Nullary Terminate

arg offset program = readMem (view ip program + offset) program

moveIP offset = over ip (+ offset)

binOp fn program =
  let arg' offset = readMem (arg offset program) program
      left = arg' 1
      right = arg' 2
      result = fn left right
   in run $ moveIP 4 $ writeMem (arg 3 program) result program

execute (Ternary Add _ _ _)  = binOp (+)
execute (Ternary Mult _ _ _) = binOp (*)
execute (Nullary Terminate)  = id

run program =
  let op = operation $ readMem (view ip program) program
   in execute op program
