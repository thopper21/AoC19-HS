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

arg offset = do
  pos <- view ip
  readMem (pos + offset)

readArg offset = do
  pos <- arg offset
  readMem pos

writeArg offset value = do
  pos <- arg offset
  writeMem pos value

next offset = run . over ip (+ offset)

ternaryOp fn = do
  left <- readArg 1
  right <- readArg 2
  next 4 . writeArg 3 (fn left right)

execute (Ternary Add _ _ _)  = ternaryOp (+)
execute (Ternary Mult _ _ _) = ternaryOp (*)
execute (Nullary Terminate)  = id

run = do
  pos <- view ip
  opCode <- readMem pos
  execute $ operation opCode
