{-# LANGUAGE TemplateHaskell #-}

module IntCode
  ( parseProgram
  , program
  , run
  , writeMem
  , readMem
  , diagnosticCode
  , setInput
  ) where

import           Control.Lens
import           Data.IntMap.Lazy (IntMap, empty, fromDistinctAscList, insert,
                                   lookup)
import           Data.List.Split  (splitOn)
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
  | Binary BinaryOp
           ParamMode
           ParamMode
  | Ternary TernaryOp
            ParamMode
            ParamMode
            ParamMode

makeLenses ''Program

emptyProgram = Program {_memory = empty, _ip = 0, _input = [], _output = []}

program input = set memory (fromDistinctAscList $ zip [0 ..] input) emptyProgram

parseProgram = program . fmap read . splitOn ","

writeMem pos = over memory . insert pos

readMem pos = fromJust . lookup pos . view memory

diagnosticCode = head . view output

setInput = set input

ternary op 000 = Ternary op Position Position Position
ternary op 001 = Ternary op Immediate Position Position
ternary op 010 = Ternary op Position Immediate Position
ternary op 011 = Ternary op Immediate Immediate Position
ternary op 100 = Ternary op Position Position Immediate
ternary op 101 = Ternary op Immediate Position Immediate
ternary op 110 = Ternary op Position Immediate Immediate
ternary op 111 = Ternary op Immediate Immediate Immediate

binary op 00 = Binary op Position Position
binary op 01 = Binary op Immediate Position
binary op 10 = Binary op Position Immediate
binary op 11 = Binary op Immediate Immediate

unary op 0 = Unary op Position
unary op 1 = Unary op Immediate

nullary op 0 = Nullary op

operator 1 = ternary Add
operator 2 = ternary Mult
operator 3 = unary In
operator 4 = unary Out
operator 5 = binary JumpIfTrue
operator 6 = binary JumpIfFalse
operator 7 = ternary LessThan
operator 8 = ternary Equals
operator 99 = nullary Terminate

operation opCode = operator (opCode `mod` 100) (opCode `div` 100)

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

incIP offset = run . over ip (+ offset)

binaryOp fn left right out = do
  x <- readArg left 1
  y <- readArg right 2
  incIP 4 . writeArg out 3 (fn x y)

fromInput argument = do
  val <- head . view input
  incIP 2 . writeArg argument 1 val . over input tail

toOutput argument = do
  val <- readArg argument 1
  incIP 2 . over output (cons val)

jump fn val out = do
  x <- readArg val 1
  pos <- readArg out 2
  if fn x
    then run . set ip pos
    else incIP 3

cmp fn left right out = do
  x <- readArg left 1
  y <- readArg right 2
  let val =
        if fn x y
          then 1
          else 0
  incIP 4 . writeArg out 3 val

execute (Ternary Add left right out)      = binaryOp (+) left right out
execute (Ternary Mult left right out)     = binaryOp (*) left right out
execute (Unary In argument)               = fromInput argument
execute (Unary Out argument)              = toOutput argument
execute (Binary JumpIfTrue compare out)   = jump (/= 0) compare out
execute (Binary JumpIfFalse compare out)  = jump (== 0) compare out
execute (Ternary LessThan left right out) = cmp (<) left right out
execute (Ternary Equals left right out)   = cmp (==) left right out
execute (Nullary Terminate)               = id

run = do
  pos <- view ip
  opCode <- readMem pos
  execute $ operation opCode
