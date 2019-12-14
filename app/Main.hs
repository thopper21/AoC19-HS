module Main where

import qualified Day1Solution

data Day =
  Day1

data Part
  = Part1
  | Part2

getInputFile Day1 = "Day1.txt"

solution Day1 Part1 = Day1Solution.solveA
solution Day1 Part2 = Day1Solution.solveB

input day = "input/" ++ getInputFile day

run day part = do
  input <- readFile $ input day
  print $ solution day part input

main :: IO ()
main = run Day1 Part2
