module Day4Solution
  ( solveA
  , solveB
  ) where

import           Data.List.HT

digits x = reverse $ digits' x
  where
    digits' 0 = []
    digits' x = (x `mod` 10) : digits' (x `div` 10)

adjacentDigits = or . mapAdjacent (==)

increasingDigits = and . mapAdjacent (<=)

matches x = adjacentDigits x && increasingDigits x

solveA _ =
  let begin = 130254
      end = 678275
   in toInteger . length . filter matches . fmap digits $ [begin .. end]
