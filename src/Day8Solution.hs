module Day8Solution
  ( solveA
  ) where

import           Data.Char
import           Data.List
import           Data.List.Split

width = 25

height = 6

pixels = width * height

countPixelsOf pixel = length . filter (== pixel)

countZeroes = countPixelsOf 0

fewestZeroes left right = compare (countZeroes left) (countZeroes right)

answer layer = countPixelsOf 1 layer * countPixelsOf 2 layer

solveA =
  toInteger .
  answer . minimumBy fewestZeroes . chunksOf pixels . fmap digitToInt
