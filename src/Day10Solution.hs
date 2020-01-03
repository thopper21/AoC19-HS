{-# LANGUAGE TupleSections #-}

module Day10Solution
  ( solveA
  , solveB
  ) where

import           Data.List

parseInput = concat . zipWith coordinates [0 ..] . lines
  where
    coordinates row line =
      (, row) . fst <$> filter ((== '#') . snd) (zip [0 ..] line)

direction (x, y) (x', y') =
  let dx = x' - x
      dy = y' - y
      divisor = gcd dx dy
   in (dx `div` divisor, dy `div` divisor)

countUnique = length . group . sort

uniqueDirections positions position =
  countUnique $ direction position <$> filter (/= position) positions

solveA input =
  let positions = parseInput input
   in maximum $ uniqueDirections positions <$> positions

solveB _ = 42
