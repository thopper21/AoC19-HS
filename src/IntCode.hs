{-# LANGUAGE TemplateHaskell #-}

module IntCode
  ( program
  , run
  , writeMem
  , readMem
  , diagnosticCode
  , setInput
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

data NullaryOp =
  Terminate

data UnaryOp
  = In
  | Out

data BinaryOp
  = JumpIfTrue
  | JumpIfFalse
  
data TernaryOp
  = Add
  | Mult
  | LessThan
  | Equals

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

diagnosticCode = head . view output

setInput = set input

operation 00001 = Ternary Add Position Position Position
operation 00101 = Ternary Add Immediate Position Position
operation 01001 = Ternary Add Position Immediate Position
operation 01101 = Ternary Add Immediate Immediate Position
operation 00002 = Ternary Mult Position Position Position
operation 00102 = Ternary Mult Immediate Position Position
operation 01002 = Ternary Mult Position Immediate Position
operation 01102 = Ternary Mult Immediate Immediate Position
operation 003   = Unary In Position
operation 004   = Unary Out Position
operation 104   = Unary Out Immediate
operation 99    = Nullary Terminate

arg offset = do
  pos <- view ip
  readMem (pos + offset)

readArg Immediate offset = arg offset
readArg Position offset = do
  pos <- arg offset
  readMem pos

writeArg Position offset value = do
  pos <- arg offset
  writeMem pos value

next offset = run . over ip (+ offset)

ternaryOp fn left right out = do
  x <- readArg left 1
  y <- readArg right 2
  next 4 . writeArg out 3 (fn x y)

fromInput argument = do
  val <- head . view input
  next 2 . writeArg argument 1 val . over input tail

toOutput argument = do
  val <- readArg argument 1
  next 2 . over output (cons val)

execute (Ternary Add left right out)  = ternaryOp (+) left right out
execute (Ternary Mult left right out) = ternaryOp (*) left right out
execute (Unary In argument)           = fromInput argument
execute (Unary Out argument)          = toOutput argument
execute (Nullary Terminate)           = id

run = do
  pos <- view ip
  opCode <- readMem pos
  execute $ operation opCode
