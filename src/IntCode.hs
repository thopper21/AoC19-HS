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
  , _outputValues :: [Integer]
  , _relativeBase :: Integer
  }

data ParamMode
  = Immediate
  | Position
  | Relative

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

lastOutput = head . view outputValues

incIP = modify $ over ip (+ 1)

nextOp = do
  incIP
  continue

readIP = do
  pos <- getIP
  readMem pos
  where
    getIP = gets $ view ip

nextArg = do
  incIP
  readIP

nextRelativeOffset = do
  relativeOffset <- nextArg
  relativeBase <- getRelativeBase
  return $ relativeBase + relativeOffset
  where
    getRelativeBase = gets $ view relativeBase

readNext Immediate = nextArg
readNext Position = do
  pos <- nextArg
  readMem pos
readNext Relative = do
  pos <- nextRelativeOffset
  readMem pos

writeNext Position value = do
  pos <- nextArg
  writeMem pos value
writeNext Relative value = do
  pos <- nextRelativeOffset
  writeMem pos value

binaryOp op leftParam rightParam outParam = do
  left <- readNext leftParam
  right <- readNext rightParam
  let result = left `op` right
  writeNext outParam result
  nextOp

add = binaryOp (+)

mult = binaryOp (*)

input outParam = gets $ AwaitingInput . continuation
  where
    resume val = do
      writeNext outParam val
      nextOp
    continuation = flip $ runState . resume

output inParam = do
  val <- readNext inParam
  outputValue val
  nextOp
  where
    outputValue = modify . over outputValues . cons

jump jumpIf valParam posParam = do
  val <- readNext valParam
  pos <- readNext posParam
  if jumpIf val
    then do
      setIP pos
      continue
    else nextOp
  where
    setIP = modify . set ip

jumpIfTrue = jump (/= 0)

jumpIfFalse = jump (== 0)

cmp fn leftParam rightParam outParam = do
  left <- readNext leftParam
  right <- readNext rightParam
  let out = outValue $ fn left right
  writeNext outParam out
  nextOp
  where
    outValue True  = 1
    outValue False = 0

lessThan = cmp (<)

equals = cmp (==)

adjustRelativeBase param = do
  val <- readNext param
  adjustBy val
  nextOp
  where
    adjustBy = modify . over relativeBase . (+)

terminate = return Terminated

param = do
  result <- gets (`mod` 10)
  modify (`div` 10)
  return $ paramMode result
  where
    paramMode 0 = Position
    paramMode 1 = Immediate
    paramMode 2 = Relative

ternary op = op <$> param <*> param <*> param

binary op = op <$> param <*> param

unary op = op <$> param

nullary = return

operator 1  = ternary add
operator 2  = ternary mult
operator 3  = unary input
operator 4  = unary output
operator 5  = binary jumpIfTrue
operator 6  = binary jumpIfFalse
operator 7  = ternary lessThan
operator 8  = ternary equals
operator 9  = unary adjustRelativeBase
operator 99 = nullary terminate

execute opCode = evalState op paramMode
  where
    op = operator $ opCode `mod` 100
    paramMode = opCode `div` 100

continue = do
  opCode <- readIP
  execute opCode

run = runState continue

runWithInput input = runWithInput' input . run
  where
    runWithInput' [] result = result
    runWithInput' (input:inputs) (AwaitingInput resume, _) =
      runWithInput' inputs $ resume input
    runWithInput' _ result@(Terminated, _) = result
