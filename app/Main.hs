module Main where

import           Day1

run file fn = do
    input <- readFile file
    let result = fn input
    print result

day1 = run "input/Day1.txt" Day1.solveA

main :: IO ()
main = day1
