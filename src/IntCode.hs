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
operation 0005  = Binary JumpIfTrue Position Position
operation 0105  = Binary JumpIfTrue Immediate Position
operation 1005  = Binary JumpIfTrue Position Immediate
operation 1105  = Binary JumpIfTrue Immediate Immediate
operation 0006  = Binary JumpIfFalse Position Position
operation 0106  = Binary JumpIfFalse Immediate Position
operation 1006  = Binary JumpIfFalse Position Immediate
operation 1106  = Binary JumpIfFalse Immediate Immediate
operation 00007 = Ternary LessThan Position Position Position
operation 00107 = Ternary LessThan Immediate Position Position
operation 01007 = Ternary LessThan Position Immediate Position
operation 01107 = Ternary LessThan Immediate Immediate Position
operation 00008 = Ternary Equals Position Position Position
operation 00108 = Ternary Equals Immediate Position Position
operation 01008 = Ternary Equals Position Immediate Position
operation 01108 = Ternary Equals Immediate Immediate Position
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

incIP offset = run . over ip (+ offset)

ternaryOp fn left right out = do
  x <- readArg left 1
  y <- readArg right 2
  incIP 4 . writeArg out 3 (fn x y)

fromInput argument = do
  val <- head . view input
  incIP 2 . writeArg argument 1 val . over input tail

toOutput argument = do
  val <- readArg argument 1
  incIP 2 . over output (cons val)

jump :: (Int -> Bool) -> ParamMode -> ParamMode -> Program -> Program
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

execute (Ternary Add left right out)      = ternaryOp (+) left right out
execute (Ternary Mult left right out)     = ternaryOp (*) left right out
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
