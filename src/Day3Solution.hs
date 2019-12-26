module Day3Solution
  ( solveA,
    solveB
  ) where

import           Data.Maybe
import           Text.ParserCombinators.Parsec

type Magnitude = Int

data Direction
  = R
  | L
  | U
  | D

type Move = (Direction, Magnitude)

type Path = [Move]

type Input = (Path, Path)

magnitude = read <$> many (oneOf "0123456789")

direction = dir <$> oneOf "RLUD"
  where
    dir 'R' = R
    dir 'L' = L
    dir 'U' = U
    dir 'D' = D

move = (,) <$> direction <*> magnitude

path = sepBy move $ char ','

eol = char '\n'

input :: Parser Input
input = (,) <$> path <* eol <*> path <* eof

type Position = (Int, Int)

type Segment = (Position, Position)

applyMove (x, y) (R, mag) = (x + mag, y)
applyMove (x, y) (L, mag) = (x - mag, y)
applyMove (x, y) (U, mag) = (x, y + mag)
applyMove (x, y) (D, mag) = (x, y - mag)

toSegments = toSegments' (0, 0)
  where
    toSegments' _ [] = []
    toSegments' pos (move:moves) =
      let newPos = applyMove pos move
          segments = toSegments' newPos moves
       in (pos, newPos) : segments

contains left right middle =
  if left < right
    then (left < middle) && (middle < right)
    else (right < middle) && (middle < left)

intersection ((x0, y0), (x1, y1)) ((x2, y2), (x3, y3))
  | (x0 == x1) && (y2 == y3) && contains x2 x3 x0 && contains y0 y1 y2 =
    Just (x0, y2)
  | (y0 == y1) && (x2 == x3) && contains y2 y3 y0 && contains x0 x1 x2 =
    Just (x2, y0)
  | otherwise = Nothing

manhattan (x, y) = abs x + abs y

solveA s =
  let Right (first, second) = parse input "" s
      intersections =
        catMaybes
          [intersection l r | l <- toSegments first, r <- toSegments second]
   in toInteger (minimum . filter (> 0) . fmap manhattan $ intersections)

solveB s = 42
