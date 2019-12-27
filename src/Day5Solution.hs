module Day5Solution
  ( solveA
  ) where

import           Data.List.Split (splitOn)
import           IntCode

parseInput = fmap read . splitOn ","

solveA = toInteger . diagnosticCode . run . setInput [1] . program . parseInput
