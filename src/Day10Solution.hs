{-# LANGUAGE TupleSections #-}

module Day10Solution
  ( solveA
  , solveB
  ) where

import           Data.List
import           Data.Ord

parseInput = concat . zipWith coordinates [0 ..] . lines
  where
    coordinates row line =
      (, row) . fst <$> filter ((== '#') . snd) (zip [0 ..] line)

relativeTo (x, y) (x', y') =
  let dx = x' - x
      dy = y' - y
      radius = gcd dx dy
   in ((dx `div` radius, dy `div` radius), radius)

equateBy projection x y = projection x == projection y

countUnique proj = length . groupBy (equateBy proj) . sortOn proj

angle = fst

uniqueDirections positions position =
  countUnique angle $ relativeTo position <$> filter (/= position) positions

solveA input =
  let positions = parseInput input
   in maximum $ uniqueDirections positions <$> positions

theta dx dy = -atan2 (fromIntegral dx) (fromIntegral dy)

clockwisePolar ((dx, dy), radius) = (theta dx dy, radius)

relativeToPosition positions position =
  (,) position $
  groupBy (equateBy angle) $
  sortOn clockwisePolar $ relativeTo position <$> filter (/= position) positions

toAngle ((dx, dy), radius) = (theta dx dy, radius)

position (x, y) ((dx, dy), radius) = (x + dx * radius, y + dy * radius)

solveB input =
  let positions = parseInput input
      (basePosition, relativePositions) =
        maximumBy (comparing $ length . snd) $
        relativeToPosition positions <$> positions
      hits = position basePosition <$> (concat . transpose $ relativePositions)
   in hits !! 199
