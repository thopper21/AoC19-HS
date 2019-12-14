module Runner where

import qualified Day1Solution

data Day =
  Day1

data Part
  = Part1
  | Part2

toDayNumber Day1 = 1

solution Day1 Part1 = Day1Solution.solveA
solution Day1 Part2 = Day1Solution.solveB

input day = "input/Day" ++ show (toDayNumber day) ++ ".txt"

run day part = do
  input <- readFile $ input day
  print $ solution day part input
