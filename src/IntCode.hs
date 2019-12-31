{-# LANGUAGE TemplateHaskell #-}

module IntCode
  ( parseProgram
  , program
  , run
  , writeMem
  , readMem
  , lastOutput
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

data ProgramState =
  Terminated

makeLenses ''Program

emptyProgram = Program {_memory = empty, _ip = 0, _input = [], _output = []}

program input = set memory (fromDistinctAscList $ zip [0 ..] input) emptyProgram

parseProgram = program . fmap read . splitOn ","

writeMem pos = over memory . insert pos

readMem pos = fromJust . lookup pos . view memory

lastOutput = head . view output . snd

setInput = set input

paramMode 0 = Position
paramMode 1 = Immediate

param (f, x) = (f $ paramMode (x `mod` 10), x `div` 10)

ternary op params = fst . param . param . param $ (Ternary op, params)

binary op params = fst . param . param $ (Binary op, params)

unary op params = fst . param $ (Unary op, params)

nullary op 0 = Nullary op

operator 1  = ternary Add
operator 2  = ternary Mult
operator 3  = unary In
operator 4  = unary Out
operator 5  = binary JumpIfTrue
operator 6  = binary JumpIfFalse
operator 7  = ternary LessThan
operator 8  = ternary Equals
operator 99 = nullary Terminate

operation opCode = operator (opCode `mod` 100) (opCode `div` 100)

arg offset = do
  pos <- view ip
  readMem $ pos + offset

readArg Immediate offset = arg offset
readArg Position offset = do
  pos <- arg offset
  readMem pos

writeArg Immediate _ _ = error "Cannot write to immediate position"
writeArg Position offset value = do
  pos <- arg offset
  writeMem pos value

runNext offset = run . over ip (+ offset)

binaryOp op leftParam rightParam outParam = do
  left <- readArg leftParam 1
  right <- readArg rightParam 2
  runNext 4 . writeArg outParam 3 (op left right)

fromInput outParam = do
  val <- head . view input
  runNext 2 . writeArg outParam 1 val . over input tail

toOutput inParam = do
  val <- readArg inParam 1
  runNext 2 . over output (cons val)

jump jumpIf valParam posParam = do
  val <- readArg valParam 1
  pos <- readArg posParam 2
  if jumpIf val
    then run . set ip pos
    else runNext 3

cmp fn leftParam rightParam outParam = do
  left <- readArg leftParam 1
  right <- readArg rightParam 2
  let out =
        if fn left right
          then 1
          else 0
  runNext 4 . writeArg outParam 3 out

terminate program = (Terminated, program)

execute (Ternary Add left right out)      = binaryOp (+) left right out
execute (Ternary Mult left right out)     = binaryOp (*) left right out
execute (Unary In out)                    = fromInput out
execute (Unary Out val)                   = toOutput val
execute (Binary JumpIfTrue compare out)   = jump (/= 0) compare out
execute (Binary JumpIfFalse compare out)  = jump (== 0) compare out
execute (Ternary LessThan left right out) = cmp (<) left right out
execute (Ternary Equals left right out)   = cmp (==) left right out
execute (Nullary Terminate)               = terminate

exec = execute . operation

run = do
  pos <- view ip
  opCode <- readMem pos
  exec opCode
