{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module IntCode
  ( parseProgram
  , run
  , runWithInput
  , writeMemory
  , readMemory
  , lastOutput
  , ProgramState(..)
  ) where

import           Control.Lens
import           Control.Monad.State
import           Data.HashMap        (Map, empty, fromList, insert, lookup)
import           Data.List.Split     (splitOn)
import           Data.Maybe
import           Prelude             hiding (lookup)

data Program = Program
  { _memory       :: Map Integer Integer
  , _ip           :: Integer
  , _output       :: [Integer]
  , _relativeBase :: Integer
  }

data NullaryOp =
  Terminate

data UnaryOp
  = In
  | Out
  | AdjustRelativeBase

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

program input = Program memory 0 [] 0
  where
    memory = fromList $ zip [0 ..] input

parseProgram = program . fmap read . splitOn ","

writeMemory pos = over memory . insert pos

writeMem pos = modify . writeMemory pos

readMemory pos = fromMaybe 0 . lookup pos . view memory

readMem pos = gets $ readMemory pos

lastOutput = head . view output

param = do
  result <- gets (`mod` 10)
  modify (`div` 10)
  return $ paramMode result
  where
    paramMode 0 = Position
    paramMode 1 = Immediate
    paramMode 2 = Relative

ternary op = Ternary op <$> param <*> param <*> param

binary op = Binary op <$> param <*> param

unary op = Unary op <$> param

nullary op = return $ Nullary op

operator 1  = ternary Add
operator 2  = ternary Mult
operator 3  = unary In
operator 4  = unary Out
operator 5  = binary JumpIfTrue
operator 6  = binary JumpIfFalse
operator 7  = ternary LessThan
operator 8  = ternary Equals
operator 9  = unary AdjustRelativeBase
operator 99 = nullary Terminate

operation opCode = evalState op paramMode
  where
    op = operator $ opCode `mod` 100
    paramMode = opCode `div` 100

getIP = gets $ view ip

arg offset = do
  pos <- getIP
  readMem (pos + offset)

getRelativeOffset offset = do
  relativeOffset <- arg offset
  relativeBase <- getRelativeBase
  return $ relativeBase + relativeOffset
  where
    getRelativeBase = gets $ view relativeBase

readArg Immediate offset = arg offset
readArg Position offset = do
  pos <- arg offset
  readMem pos
readArg Relative offset = do
  pos <- getRelativeOffset offset
  readMem pos

writeArg Immediate _ _ = error "Cannot write to immediate position"
writeArg Position offset value = do
  pos <- arg offset
  writeMem pos value
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
      setIP pos
    else moveIP 3
  continue
  where
    setIP = modify . set ip

cmp leftParam fn rightParam outParam = do
  left <- readArg leftParam 1
  right <- readArg rightParam 2
  let out = outValue $ fn left right
  writeArg outParam 3 out
  moveIP 4
  continue
  where
    outValue True  = 1
    outValue False = 0

adjustRelativeBase param = do
  val <- readArg param 1
  adjustBy val
  moveIP 2
  continue
  where
    adjustBy = modify . over relativeBase . (+)

terminate = return Terminated

execute (Ternary Add left right out)      = binaryOp left (+) right out
execute (Ternary Mult left right out)     = binaryOp left (*) right out
execute (Unary In out)                    = fromInput out
execute (Unary Out val)                   = toOutput val
execute (Binary JumpIfTrue compare out)   = jump compare (/= 0) out
execute (Binary JumpIfFalse compare out)  = jump compare (== 0) out
execute (Ternary LessThan left right out) = cmp left (<) right out
execute (Ternary Equals left right out)   = cmp left (==) right out
execute (Unary AdjustRelativeBase val)    = adjustRelativeBase val
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
