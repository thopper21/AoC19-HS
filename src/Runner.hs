module Runner where

import qualified Day1Solution as D1
import qualified Day2Solution as D2
import qualified Day3Solution as D3

data Day
  = Day1
  | Day2
  | Day3

data Part
  = Part1
  | Part2

toDayNumber Day1 = 1
toDayNumber Day2 = 2
toDayNumber Day3 = 3

solution Day1 Part1 = D1.solveA
solution Day1 Part2 = D1.solveB
solution Day2 Part1 = D2.solveA
solution Day2 Part2 = D2.solveB
solution Day3 Part1 = D3.solveA
solution Day3 Part2 = D3.solveB

input day = "input/Day" ++ show (toDayNumber day) ++ ".txt"

run day part = do
  input <- readFile $ input day
  print $ solution day part input
