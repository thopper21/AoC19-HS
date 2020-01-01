{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module IntCode
  ( parseProgram
  , program
  , run
  , runWithInput
  , writeMemory
  , readMemory
  , lastOutput
  , ProgramState(..)
  ) where

import           Control.Lens
import           Control.Monad.State
import           Data.IntMap         (IntMap, empty, fromDistinctAscList,
                                      insert, lookup)
import           Data.List.Split     (splitOn)
import           Data.Maybe
import           Prelude             hiding (lookup)

data Program = Program
  { _memory       :: IntMap Integer
  , _ip           :: Int
  , _output       :: [Integer]
  , _relativeBase :: Int
  }

data NullaryOp =
  Terminate

data UnaryOp
  = In
  | Out
  | AddRelative

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
  | Relative

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

type ResumeProgram = Integer -> (ProgramState, Program)

data ProgramState
  = Terminated
  | AwaitingInput ResumeProgram

makeLenses ''Program

emptyProgram = Program empty 0 [] 0

program input = set memory (fromDistinctAscList $ zip [0 ..] input) emptyProgram

parseProgram = program . fmap read . splitOn ","

writeMemory pos = over memory . insert pos

writeMem pos = modify . writeMemory pos

readMemory pos = fromMaybe 0 . lookup pos . view memory

readMem pos = gets $ readMemory pos

lastOutput = head . view output

paramMode 0 = Position
paramMode 1 = Immediate
paramMode 2 = Relative

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
operator 9  = unary AddRelative
operator 99 = nullary Terminate

operation opCode = operator (opCode `mod` 100) (opCode `div` 100)

getIP = gets $ view ip

getRelativeBase = gets $ view relativeBase

arg offset = do
  pos <- getIP
  readMem (pos + offset)

getRelativeOffset offset = do
  relativeOffset <- arg offset
  relativeBase <- getRelativeBase
  return $ relativeBase + fromInteger relativeOffset

readArg Immediate offset = arg offset
readArg Position offset = do
  pos <- arg offset
  readMem . fromInteger $ pos
readArg Relative offset = do
  pos <- getRelativeOffset offset
  readMem pos

writeArg Immediate _ _ = error "Cannot write to immediate position"
writeArg Position offset value = do
  pos <- arg offset
  writeMem (fromInteger pos) value
writeArg Relative offset value = do
  pos <- getRelativeOffset offset
  writeMem pos value

moveIP offset = modify $ over ip (+ offset)

binaryOp leftParam op rightParam outParam = do
  left <- readArg leftParam 1
  right <- readArg rightParam 2
  let result = left `op` right
  writeArg outParam 3 result
  moveIP 4
  continue

fromInput outParam = gets $ AwaitingInput . continuation
  where
    resume val = do
      writeArg outParam 1 val
      moveIP 2
      continue
    continuation = flip $ runState . resume

toOutput inParam = do
  val <- readArg inParam 1
  outputValue val
  moveIP 2
  continue
  where
    outputValue = modify . over output . cons

jump valParam jumpIf posParam = do
  val <- readArg valParam 1
  if jumpIf val
    then do
      pos <- readArg posParam 2
      setIP $ fromInteger pos
    else moveIP 3
  continue
  where
    setIP = modify . set ip

cmp leftParam fn rightParam outParam = do
  left <- readArg leftParam 1
  right <- readArg rightParam 2
  let out =
        if fn left right
          then 1
          else 0
  writeArg outParam 3 out
  moveIP 4
  continue

addRelative param = do
  val <- readArg param 1
  modify $ over relativeBase (+ fromIntegral val)
  moveIP 2
  continue

terminate = return Terminated

execute (Ternary Add left right out)      = binaryOp left (+) right out
execute (Ternary Mult left right out)     = binaryOp left (*) right out
execute (Unary In out)                    = fromInput out
execute (Unary Out val)                   = toOutput val
execute (Binary JumpIfTrue compare out)   = jump compare (/= 0) out
execute (Binary JumpIfFalse compare out)  = jump compare (== 0) out
execute (Ternary LessThan left right out) = cmp left (<) right out
execute (Ternary Equals left right out)   = cmp left (==) right out
execute (Unary AddRelative val)           = addRelative val
execute (Nullary Terminate)               = terminate

continue = do
  pos <- getIP
  opCode <- readMem pos
  execute . operation $ opCode

run = runState continue

runWithInput input = runWithInput' input . run
  where
    runWithInput' [] result = result
    runWithInput' (input:inputs) (AwaitingInput resume, _) =
      runWithInput' inputs $ resume input
    runWithInput' _ result@(Terminated, _) = result
