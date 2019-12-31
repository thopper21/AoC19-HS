module Day3Solution
  ( solveA
  , solveB
  ) where

import           Data.List.HT
import           Data.Maybe
import           Text.ParserCombinators.Parsec

type Magnitude = Int

data Direction
  = R
  | L
  | U
  | D
  deriving (Eq)

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

applyMove (x, y) (dir, mag) =
  case dir of
    R -> (x + mag, y)
    L -> (x - mag, y)
    U -> (x, y + mag)
    D -> (x, y - mag)

toPositions = scanl applyMove (0, 0)

toSegments = mapAdjacent (,) . toPositions

within middle (left, right) =
  if left < right
    then (left < middle) && (middle < right)
    else (right < middle) && (middle < left)

intersects ((x0, y0), (x1, y1)) ((x2, y2), (x3, y3)) =
  (y2 `within` (y0, y1) && x0 `within` (x2, x3)) ||
  (y0 `within` (y2, y3) && x2 `within` (x0, x1))

-- Precondition - intersects s0 s1 == True
intersection ((x0, y0), (x1, y1)) ((x2, y2), (x3, y3))
  | x0 == x1 = (x0, y2)
  | otherwise = (x2, y0)

manhattan (x, y) = abs x + abs y

fromRight either =
  case either of
    Right x -> x

-- Assume input is well-formed
parseInput = fromRight . parse input ""

solveA s =
  let (p0, p1) = parseInput s
      intersections =
        [ intersection s0 s1
        | s0 <- toSegments p0
        , s1 <- toSegments p1
        , intersects s0 s1
        ]
   in minimum . filter (> 0) . fmap manhattan $ intersections

toStepPositions = scanl stepPositions (0, (0, 0))
  where
    stepPositions (steps, pos) move = (steps + snd move, applyMove pos move)

toStepSegments = mapAdjacent f . toStepPositions
  where
    f (steps, pos0) (_, pos1) = (steps, (pos0, pos1))

steps (steps0, p0@((x0, y0), _)) (steps1, p1@((x1, y1), _)) =
  let (x, y) = intersection p0 p1
   in steps0 + steps1 + abs (x0 - x) + abs (x1 - x) + abs (y0 - y) +
      abs (y1 - y)

solveB s =
  let (p0, p1) = parseInput s
      allSteps =
        [ steps s0 s1
        | s0 <- toStepSegments p0
        , s1 <- toStepSegments p1
        , intersects (snd s0) (snd s1)
        ]
   in minimum . filter (> 0) $ allSteps
