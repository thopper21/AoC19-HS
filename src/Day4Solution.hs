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

matchA x = adjacentDigits x && increasingDigits x

begin = 130254

end = 678275

solveA _ = length . filter matchA . fmap digits $ [begin .. end]

exactAdjacent = elem 2 . fmap length . group

matchB x = exactAdjacent x && increasingDigits x

solveB _ = length . filter matchB . fmap digits $ [begin .. end]
