module Main where

import           Day1

run file fn = do
  input <- readFile file
  let result = fn input
  print result

day1 = run "input/Day1.txt"

day1A = day1 Day1.solveA

day1B = day1 Day1.solveB

main :: IO ()
main = day1B
