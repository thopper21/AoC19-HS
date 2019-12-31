module Day8Solution
  ( solveA
  , solveB
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

layers = chunksOf pixels . fmap digitToInt

solveA = answer . minimumBy fewestZeroes . layers

data PixelColor
  = Black
  | White
  | Transparent

intToPixelColor 0 = Black
intToPixelColor 1 = White
intToPixelColor 2 = Transparent

pixelColorToChar Black       = ' '
pixelColorToChar White       = 'X'
pixelColorToChar Transparent = ' '

pixelColor = pixelColorToChar . foldl toColor Transparent
  where
    toColor Black _           = Black
    toColor White _           = White
    toColor Transparent color = intToPixelColor color

solveB = unlines . chunksOf width . fmap pixelColor . transpose . layers
