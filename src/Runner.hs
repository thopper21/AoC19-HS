module Runner where

import qualified Day1Solution as D1
import qualified Day2Solution as D2
import qualified Day3Solution as D3
import qualified Day4Solution as D4
import qualified Day5Solution as D5
import qualified Day6Solution as D6
import qualified Day7Solution as D7

data Day
  = Day1
  | Day2
  | Day3
  | Day4
  | Day5
  | Day6
  | Day7

data Part
  = Part1
  | Part2

toDayNumber Day1 = 1
toDayNumber Day2 = 2
toDayNumber Day3 = 3
toDayNumber Day4 = 4
toDayNumber Day5 = 5
toDayNumber Day6 = 6
toDayNumber Day7 = 7

solution Day1 Part1 = D1.solveA
solution Day1 Part2 = D1.solveB
solution Day2 Part1 = D2.solveA
solution Day2 Part2 = D2.solveB
solution Day3 Part1 = D3.solveA
solution Day3 Part2 = D3.solveB
solution Day4 Part1 = D4.solveA
solution Day4 Part2 = D4.solveB
solution Day5 Part1 = D5.solveA
solution Day5 Part2 = D5.solveB
solution Day6 Part1 = D6.solveA
solution Day6 Part2 = D6.solveB
solution Day7 Part1 = D7.solveA

input day = "input/Day" ++ show (toDayNumber day) ++ ".txt"

run day part = do
  input <- readFile $ input day
  print $ solution day part input
